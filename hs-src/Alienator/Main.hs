{-# LANGUAGE LambdaCase #-}
module Alienator.Main
  (
    main
  ) where

import Control.Monad
import Control.Monad.Trans
import Control.Lens hiding ((#))
import Reflex
import Reflex.Cocos2d
import Reflex.State
import Reflex.Extra

import Linear.Affine
import Diagrams (unr2, V2(..))
import Diagrams.TwoD.Shapes
import Control.Monad.Trans.Free

import Alienator.GamePlayScene
import Alienator.State
import Alienator.Constants

data Scene = StartScene | GamePlayScene | GameOverScene deriving (Show, Read, Eq)

renderScene :: V2 Float -> Scene -> FreeT (Event Spider) SpiderNodeBuilder ()
renderScene winSize StartScene = do
  (_, clicked) <- lift $ button
    [ titleText       := "Start"
    , titleFontSize   := 20
    , position        := P (winSize/2)
    ]
  waitEvent_ clicked
  renderScene winSize GamePlayScene
renderScene winSize GamePlayScene = do
  keysDyn <- lift $ getKeyboardEvents >>= accumKeysDown
  waitEvent_ <=< lift $ do
    debug "playing game!"
    (sp, steps) <- space [ iterations := 2 ]
    collisionEvts <- getCollisionEvents sp
    let collisionsE = fanCollisionsByBody (collisionEvts^.collisionBegan)
    -- walls
    debug "putting up walls"
    wb <- body sp [ position := P (winSize/2) ]
    let rectPts = uncurry rect $ unr2 (winSize + 100)
    forM_ (zip rectPts $ tail $ cycle rectPts) $ \(a, b) ->
      void $ shape sp wb (LineSegment a b 100)
        [ active   := True
        , category := Wall
        ]
    debug "putting up scene"
    flip evalAccStateT (initGamePlaySceneState winSize) $
      gamePlayScene winSize sp steps collisionsE keysDyn
  renderScene winSize GameOverScene
renderScene winSize GameOverScene = do
  (_, clicked) <- lift $ button
    [ titleText       := "Game Over"
    , titleFontSize   := 50
    , position        := P (winSize/2)
    ]
  waitEvent_ clicked
  renderScene winSize StartScene

main :: IO ()
main = mainScene $ do
    winSize <- getWindowSize
    void . runWithReplaceFree $ renderScene winSize StartScene
