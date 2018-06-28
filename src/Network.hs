{-# LANGUAGE Arrows        #-}
{-# LANGUAGE TupleSections #-}

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

data ShipEvent = ShipTop | ShipBottom | ShipFlap

network' :: RandomGen g => g -> Game -> SF Controller (Game, Event g)
network' gen game = proc ctrl -> do
  rec
    flapped <- shipFlapBounded game -< (game, ctrl)
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

    stopped <- stopGame -< (ctrl, gamePlaying)
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

shipFlapBounded' :: Ship
  -> SF (Game, Controller) (Ship, Event (ShipEvent, Game))
shipFlapBounded' s = proc (g, c) -> do
  flapped <- shipFall s -< ()
  shipFlap <- edgeTag ShipFlap -< c ^. controllerFlap
  let sr = flapped ^. shipSprite . spriteRect
  shipBottom <- edgeTag ShipBottom
    -< sr ^. rectP . _y + sr ^. rectD . _y > g ^. gameBounds . _y
  shipTop <- edgeTag ShipTop -< sr ^. rectP . _y < 0
  returnA -<
    ( flapped
    , (, g & gameShip .~ flapped)
      <$> shipFlap `lMerge` shipBottom `lMerge` shipTop)
-- | Make the ship fall, responding to hitting the bottom, top, or
-- flap being pressed.
shipFlapBounded :: Game -> SF (Game, Controller) Ship
shipFlapBounded = flip switch (uncurry handleEvent) . shipFlapBounded'
  . view gameShip
  where
    handleEvent ShipFlap g' = shipFlapBounded
      $ g' & gameShip . shipV .~ - g' ^. gameShip . shipVT
    handleEvent ShipBottom g' = shipFlapBounded $ execState
      ( gameShip . shipV .= 0
        >> gameShip . shipSprite . spriteRect . rectP . _y
        .= g' ^. gameBounds . _y
        - g' ^. gameShip . shipSprite . spriteRect . rectD . _y
      ) g'
    handleEvent ShipTop g' = shipFlapBounded $ execState
      ( gameShip . shipV .= 0
        >> gameShip . shipSprite . spriteRect . rectP . _y .= 0
      ) g'

shipCollide :: (Functor f, Foldable f) => (AsteroidBelt -> f Asteroid)
  -> SF Game (Game, Event Game)
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
-- stopGame :: SF Game Game
-- stopGame = switch shipCollideWrong $ \g -> constant $ (gameOver .~ True) g
stopGame :: SF (Controller, Game) Game
stopGame = switch (snd ^>> shipCollideWrong) $ \g -> proc (ctrl, _) ->
  returnA -< execState
    ( gameQuit .= ctrl ^. controllerQuit
    >> gameOver .= True
    >> gameOverMsg . textBoxColor . _w .= 255
    ) g
