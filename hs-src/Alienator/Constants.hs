{-# LANGUAGE DeriveAnyClass #-}
module Alienator.Constants
  (
    CollisionCategory(..)
  ) where

import Reflex.Cocos2d.Chipmunk

data CollisionCategory =
      Wall
    | PlayerBullet
    | EnemyBullet
    | PlayerShip
    | EnemyShip
   deriving (Enum, Show, Read, Eq, Maskable)

