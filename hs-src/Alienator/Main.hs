{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Alienator.Main
  (
    main
  ) where

import Data.Default

import Control.Monad
import Control.Monad.Trans
import Control.Lens hiding ((#))
import Reflex
import Reflex.Cocos2d
import Reflex.State

import Linear.Affine
import Diagrams (unr2, translate, (#))
import Diagrams.Points
import Diagrams.TwoD.Shapes
import qualified Diagrams.BoundingBox as B

import Alienator.GamePlayScene
import Alienator.State
import Alienator.Constants


data Scene = StartScene | GamePlayScene | GameOverScene deriving (Show, Read, Eq)

main :: IO ()
main = mainScene $ do
    winSize <- view windowSize
    ticks <- view frameTicks
    keysDyn <- getKeyboardEvents >>= accumKeysDown
    touchEs <- getTouchEvents

    (sp, steps) <- space [ iterations := 2 ]
    collisionEvts <- getCollisionEvents sp
    let collisionsE = fanCollisionsByBody (collisionEvts^.collisionBegan)
    -- walls
    wb <- body sp [ position := P (winSize/2) ]
    let rectPts = uncurry rect $ unr2 (winSize + 100)
    forM (zip rectPts $ tail $ cycle rectPts) $ \(a, b) ->
      void $ shape sp wb (LineSegment a b 100)
        [ active   := True
        , category := Wall
        ]
    let halfsize = winSize/2
    void $ runAccStateT ?? StartScene $ do
      vDyn <- watches $ \case
        StartScene -> do
          l <- label [ position       := P halfsize
                     , text           := "Start"
                     , systemFontSize := 20
                     ]
          sz <- get l contentSize
          liftIO $ putStrLn $ "got size of label " ++ show sz
          let (wid, height) = unr2 sz
              bbox = B.fromPoints $ rect wid height & translate halfsize
              touched = ffilter (B.contains bbox . (^.touchLocation)) (touchEs^.touchBegan)
          adjust $ const GamePlayScene <$ touched
        GamePlayScene -> do
          liftIO $ putStrLn "playing game!"
          overE <- flip evalAccStateT (initGamePlaySceneState winSize) $ do
            gamePlayScene winSize sp steps collisionsE keysDyn
          adjust $ const GameOverScene <$ overE
        GameOverScene -> do
          l <- label [ text           := "Game Over"
                     , systemFontSize := 50
                     , position       := 0 .+^ winSize/2
                     ]
          sz <- get l contentSize
          let bbox = B.fromPoints $ uncurry rect (unr2 sz) # translate halfsize
              touched = ffilter (B.contains bbox . (^.touchLocation)) (touchEs^.touchBegan)
          adjust $ const StartScene <$ touched
      node [] -<< vDyn
