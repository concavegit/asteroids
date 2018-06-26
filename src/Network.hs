{-# LANGUAGE Arrows #-}

module Network
  ( network
  ) where

import           Control.Arrow
import           Control.Lens
import           Control.Monad.State
import           Data.Bool
import           Data.Either
import           FRP.Yampa
import           Network.Extra
import           SDL                 hiding (Event)
import           Types

network' :: RandomGen g => g -> Game -> SF Controller (Game, Event g)
network' gen game = proc ctrl -> do
  rec
    flapped <- shipBounded game -< ctrl
    multGen <- noiseR (game ^. gameMultRange) (fst $ split gen) -< ()

    let generate = genAsteroids (mult' ^. multChoices) (game ^. gameBounds . _y)
          $ forward ^. asteroidBeltHead
        vMult = generate ^. asteroidBeltHead . asteroidVMult <$ astersEnd

    accel <- astersAccel
      $ game ^. gameAsteroidBelt . asteroidBeltHead . asteroidV
      -< (generate, vMult)

    forward <- astersForward (game ^. gameAsteroidBelt)
      >>> iPre (game ^. gameAsteroidBelt) -< accel

    collideRight <- shipCollideRight >>> snd ^>> iPre noEvent -< gamePlaying

    astersSwitch <- (False <$) *** (True <$) ^>> uncurry lMerge ^>> hold False
      -< (astersEnd, collideRight)
    let noRights = filter isLeft forward
        astersSwitched = bool forward noRights astersSwitch

    offset <- accumHold 0
      -< ( + forward
         ^. asteroidBeltHead . asteroidSprite . spriteRect . rectD . _x
        ) . (+ game ^. gameBounds . _x) <$ astersEnd

    let looped = eitherFmap (asteroidSprite . spriteRect . rectP . _x +~ offset)
          astersSwitched


    astersEnd <- astersAroundE
      ( game ^. gameAsteroidBelt . asteroidBeltHead . asteroidSprite
        . spriteRect . rectP . _x
      ) -< game & gameAsteroidBelt .~ forward

    init  <- now () -< ()
    mult' <- hold (game ^. gameMultObj . multObjMult) -< multGen
      <$ tag astersEnd () `lMerge` init

    score' <- accumHold 0 -< (+1) <$ collideRight

    let gamePlaying = execState
          ( gameShip .= flapped
            >> gameAsteroidBelt .= looped
            >> gameMultObj . multObjMult .= mult'
            >> gameScore . score .= score'
            >> gameQuit .= ctrl ^. controllerQuit
          ) game

    restartPressed <- edge -< ctrl ^. controllerFlap

    stopped <- stopGame -< gamePlaying
    let restart = gate restartPressed (stopped ^. gameOver)

  returnA -< (stopped, snd (split gen) <$ restart)

-- | Restart the game when commanded.
network :: RandomGen a => a -> Game -> SF Controller Game
network gen game = switch (network' gen game) $ flip network game

-- | Move the asteroids forward.
astersForward :: AsteroidBelt -> SF AsteroidBelt AsteroidBelt
astersForward belt0 = proc belt -> do
  move <- integral >>^ (asteroidSprite . spriteRect . rectP . _x .~)
    . (+ belt0 ^. asteroidBeltHead . asteroidSprite . spriteRect . rectP . _x)
    -< belt ^. asteroidBeltHead . asteroidV
  returnA -< eitherFmap move belt

-- | Trigger event when 'astersForward' has moved the input distance
-- plus the game's width.
gameBoundTraversed :: Double -> SF Game (Event Game)
gameBoundTraversed d = proc g -> do
  x <- integral >>^ (+ d)
    -< (g ^. gameAsteroidBelt . asteroidBeltHead . asteroidV)
  e <- edge -< x
    + ( g ^. gameAsteroidBelt . asteroidBeltHead . asteroidSprite . spriteRect
       . rectD . _x
      ) <= 0
  returnA -< g <$ e

-- | Trigger event when 'astersForward' has moved a multiple of the
-- game's width forward.'
astersAroundE :: Double -> SF Game (Event Game)
astersAroundE d = dSwitch (gameBoundTraversed d >>^ dup)
  $ astersAroundE . (^. gameBounds . _x)

-- | When an event is triggered, scale the 'AsteroidBelt's velocity by
-- '_asteroidVMult'.
astersAccel :: Double -> SF (AsteroidBelt, Event b) [Either Asteroid Asteroid]
astersAccel d = proc (a, e) -> do
  v <- accumHold d -< (* a ^. asteroidBeltHead . asteroidVMult) <$ e
  returnA -< eitherFmap (asteroidV .~ v) a

-- | Make the ship fall.
shipFall :: Ship -> SF a Ship
shipFall ship = proc _ -> do
  rec
    let a = g - v * g / vT
    v <- integral >>^ (+ ship ^. shipV) -< a
    y <- integral >>^ (+ ship ^. shipSprite . spriteRect . rectP . _y) -< v
  returnA -< execState (shipV .= y >> shipSprite . spriteRect . rectP . _y .= y)
    ship

  where
    g = ship ^. shipG
    vT = ship ^. shipVT

shipFlap' :: Ship -> SF Controller (Ship, Event Ship)
shipFlap' = (>>^ \(a, b) -> (a, a <$ b)) . (&&& (view controllerFlap ^>> edge))
  . shipFall

-- | While falling, give the ship an upwards velocity of '_shipVT'
-- when commanded.
shipFlap :: Ship -> SF Controller Ship
shipFlap ship0 = switch (shipFlap' ship0)
  $ shipFlap . ((&) <*> set shipV . negate . view shipVT)

stopShip' :: Double -> Ordering -> (a -> SF b Ship) -> a
  -> SF b (Ship, Event Ship)
stopShip' bound comp
  = ((>>> edgeCond
      ((== comp) . flip compare bound
       . view (shipSprite . spriteRect . rectP . _y))) .)

-- | Stop the ship when it compares to some distance.
-- For inputs d, o, f, x:
--   * d is the distance
--   * o is the 'Ordering' to trigger a stop when comparing the ship
--     to the distance.
--   * f is a signal that generates a ship given an initial condition.
--   * x is the initial condition.
stopShip :: Double -> Ordering -> (Ship -> SF a Ship) -> Ship -> SF a Ship
stopShip bound comp = switch1 (stopShip' bound comp)
  $ execState (shipSprite . spriteRect . rectP . _y .= bound >> shipV .= 0)

-- | Stop the ship at the bottom of the game bounds.
shipBottom :: Game -> SF Controller Ship
shipBottom game = stopShip
  ( game ^. gameBounds . _y - game ^. gameShip . shipSprite . spriteRect . rectD
   . _y
  ) GT shipFlap (game ^. gameShip)

-- | Stop the ship before it exits the game bounds.
shipBounded :: Game -> SF Controller Ship
shipBounded game = stopShip 0 LT (shipBottom . ($ game) . set gameShip)
  (game ^. gameShip)

shipCollide :: (Functor f, Foldable f) => (AsteroidBelt -> f Asteroid) -> SF Game (Game, Event Game)
shipCollide f = edgeCond $ or
  . ( fmap . rectCollide <$> (^. gameShip . shipSprite . spriteRect)
     <*> fmap (^. asteroidSprite . spriteRect) . f . view gameAsteroidBelt
    )

-- | Generate an event when the ship collides with a wrong answer.
shipCollideWrong :: SF Game (Game, Event Game)
shipCollideWrong = shipCollide lefts

-- | Generate an event when the ship collides with a correct answer.
shipCollideRight :: SF Game (Game, Event Game)
shipCollideRight = shipCollide rights

-- | Stop the game when the ship collides with a wrong answer.
stopGame :: SF Game Game
stopGame = switch shipCollideWrong $ constant . (gameOver .~ True)
