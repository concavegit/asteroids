{-# LANGUAGE Arrows #-}

module Network
  ( network
  ) where

import           Control.Arrow
import           Control.Lens
import           Control.Monad.State
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

    offset <- accumHold 0
      -< ( + forward
         ^. asteroidBeltHead . asteroidSprite . spriteRect . rectD . _x
        ) . (+ game ^. gameBounds . _x) <$ astersEnd

    let looped = eitherFmap (asteroidSprite . spriteRect . rectP . _x +~ offset)
          forward


    astersEnd <- astersAroundE
      ( game ^. gameAsteroidBelt . asteroidBeltHead . asteroidSprite
        . spriteRect . rectP . _x
      ) -< game & gameAsteroidBelt .~ forward

    init  <- now () -< ()
    mult' <- hold (game ^. gameMultObj . multObjMult) -< multGen
      <$ tag astersEnd () `lMerge` init

    score' <- accumHold 0 -< (+1) <$ astersEnd

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

network :: RandomGen a => a -> Game -> SF Controller Game
network gen game = switch (network' gen game) $ flip network game

astersForward :: AsteroidBelt -> SF AsteroidBelt AsteroidBelt
astersForward belt0 = proc belt -> do
  move <- integral >>^ (asteroidSprite . spriteRect . rectP . _x .~)
    . (+ belt0 ^. asteroidBeltHead . asteroidSprite . spriteRect . rectP . _x)
    -< belt ^. asteroidBeltHead . asteroidV
  returnA -< eitherFmap move belt

gameBoundTraversed :: Double -> SF Game (Event Game, Event Game)
gameBoundTraversed d = proc g -> do
  x <- integral >>^ (+ d)
    -< (g ^. gameAsteroidBelt . asteroidBeltHead . asteroidV)
  e <- edge -< x
    + ( g ^. gameAsteroidBelt . asteroidBeltHead . asteroidSprite . spriteRect
       . rectD . _x
      ) <= 0
  returnA -< dup $ g <$ e

astersAroundE :: Double -> SF Game (Event Game)
astersAroundE d = dSwitch (gameBoundTraversed d)
  $ astersAroundE . (^. gameBounds . _x)

astersAccel :: Double -> SF (AsteroidBelt, Event Double) AsteroidBelt
astersAccel d = proc (a, e) -> do
  v <- accumHold d -< (* a ^. asteroidBeltHead . asteroidVMult) <$ e
  returnA -< eitherFmap (asteroidV .~ v) a

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

shipFlap :: Ship -> SF Controller Ship
shipFlap ship0 = switch (shipFlap' ship0)
  $ shipFlap . ((&) <*> set shipV . negate . view shipVT)

stopShip' :: Double -> Ordering -> (a -> SF b Ship) -> a
  -> SF b (Ship, Event Ship)
stopShip' bound comp
  = ((>>> edgeCond
      ((== comp) . flip compare bound
       . view (shipSprite . spriteRect . rectP . _y))) .)

stopShip :: Double -> Ordering -> (Ship -> SF a Ship) -> Ship -> SF a Ship
stopShip bound comp = switch1 (stopShip' bound comp)
  $ execState (shipSprite . spriteRect . rectP . _y .= bound >> shipV .= 0)

shipBottom :: Game -> SF Controller Ship
shipBottom game = stopShip
  ( game ^. gameBounds . _y - game ^. gameShip . shipSprite . spriteRect . rectD
   . _y
  ) GT shipFlap (game ^. gameShip)

shipBounded :: Game -> SF Controller Ship
shipBounded game = stopShip 0 LT (shipBottom . ($ game) . set gameShip)
  (game ^. gameShip)

shipCollide :: (Functor f, Foldable f) => (AsteroidBelt -> f Asteroid) -> SF Game (Game, Event Game)
shipCollide f = edgeCond $ or
  . ( fmap . rectCollide <$> (^. gameShip . shipSprite . spriteRect)
     <*> fmap (^. asteroidSprite . spriteRect) . f . view gameAsteroidBelt
    )

shipCollideWrong :: SF Game (Game, Event Game)
shipCollideWrong = shipCollide lefts

shipCollideRight :: SF Game (Game, Event Game)
shipCollideRight = shipCollide rights

stopGame :: SF Game Game
stopGame = switch shipCollideWrong $ constant . (gameOver .~ True)
