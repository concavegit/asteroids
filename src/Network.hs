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

network :: RandomGen g => g -> Game -> SF Controller Game
network gen game = proc ctrl -> do
  flapped <- shipBounded game -< ctrl
  (looped, loopE) <- asteroidBeltLoop game -< ()

  score' <- accumHold (game ^. gameScore . score) -< (+1) <$ loopE
  stopped <- stopGame -< execState
    ( gameScore . score .= score'
    >> gameShip .= flapped
    >> gameAsteroidBelt .= looped
    ) game

  returnA -< execState
    ( gameQuit .= ctrl ^. controllerQuit
    ) stopped

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

asteroidBeltForward :: AsteroidBelt -> SF a AsteroidBelt
asteroidBeltForward belt = proc _ -> integral
  >>^ flip eitherFmap belt . (asteroidSprite . spriteRect . rectP . _x .~)
  . (+ aster ^. asteroidSprite . spriteRect . rectP . _x) -< aster ^. asteroidV
  where aster = either id id $ head belt

asteroidBeltEnd :: AsteroidBelt -> SF a ((AsteroidBelt, Event AsteroidBelt), Event AsteroidBelt)
asteroidBeltEnd
  = (>>> edgeCond
      ((((< 0) .) . (+) <$> view (rectP . _x) <*> view (rectD . _x))
       . (^. asteroidSprite . spriteRect) . either id id . head) >>^ ((,) <*> snd)) . asteroidBeltForward

asteroidBeltLoop :: Game -> SF a (AsteroidBelt, Event AsteroidBelt)
asteroidBeltLoop game0 = dSwitch (asteroidBeltEnd $ game0 ^. gameAsteroidBelt)
  (\asters -> asteroidBeltLoop $ game0
    & gameAsteroidBelt .~ eitherFmap
    (execState
     ( asteroidSprite . spriteRect . rectP . _x .= game0 ^. gameBounds . _x
       >> asteroidV *= either id id (head asters) ^. asteroidVMult
     )) asters
  )

shipCollideWrong :: SF Game (Game, Event Game)
shipCollideWrong = edgeCond $ or . uncurry fmap . (rectCollide . (^. gameShip . shipSprite . spriteRect) &&& map (^. asteroidSprite . spriteRect) . lefts . view gameAsteroidBelt)

stopGame :: SF Game Game
stopGame = switch shipCollideWrong $ constant . (gameOver .~ True)
