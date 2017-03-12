{-# LANGUAGE DeriveAnyClass #-}
module Alienator.Constants
  (
    CollisionCategory(..)
  ) where

import Reflex.Cocos2d.Misc.Chipmunk

data CollisionCategory =
      Wall
    | PlayerBullet
    | EnemyBullet
    | PlayerShip
    | EnemyShip
   deriving (Enum, Show, Read, Eq, Maskable)

