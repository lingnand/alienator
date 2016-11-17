{-# LANGUAGE RankNTypes #-}
module Alienator.State
    ( PSpriteState'
    , BulletState
    , Meter(..)
    , meterRead
    , meterMax
    , PlayerShipState(..)
    , health
    , attack
    , defense
    , pSprite
    , GamePlaySceneState(..)
    , bulletPool
    , playerShip
    , enemyShipPool
    , initPlayerShipState
    , initBulletPool
    , initGamePlaySceneState
    , bullet
    )
  where

import Data.Default
import Diagrams (unr2, (^&), P2)
import Diagrams.TwoD.Shapes
import Linear
import Linear.Affine
import Control.Lens
import qualified Alienator.Pool as P
import Alienator.PhysicsSprite
import Alienator.Actuator
import Alienator.Constants
import Reflex.Cocos2d

type PSpriteState' ac = PhysicsSpriteState ac CollisionCategory
type BulletState m = PSpriteState' (AnyActuator m)

data Meter a = Meter { _meterRead :: a , _meterMax :: a } deriving (Show, Read, Eq)
meterMax :: forall a_a4Tw. Lens' (Meter a_a4Tw) a_a4Tw
meterMax f_aaXh (Meter x1_aaXi x2_aaXj)
  = fmap (\ y1_aaXk -> Meter x1_aaXi y1_aaXk) (f_aaXh x2_aaXj)
{-# INLINE meterMax #-}
meterRead :: forall a_a4Tw. Lens' (Meter a_a4Tw) a_a4Tw
meterRead f_aaXl (Meter x1_aaXm x2_aaXn)
  = fmap (\ y1_aaXo -> Meter y1_aaXo x2_aaXn) (f_aaXl x1_aaXm)
{-# INLINE meterRead #-}

data PlayerShipState = PlayerShipState
    { _health  :: Meter Int
    , _attack  :: Float
    , _defense :: Float
    , _pSprite :: PSpriteState' VelActuator
    } deriving (Show, Eq)

attack :: Lens' PlayerShipState Float
attack f_a9Xd (PlayerShipState x1_a9Xe x2_a9Xf x3_a9Xg x4_a9Xh)
  = fmap
      (\ y1_a9Xi -> PlayerShipState x1_a9Xe y1_a9Xi x3_a9Xg x4_a9Xh)
      (f_a9Xd x2_a9Xf)
{-# INLINE attack #-}
defense :: Lens' PlayerShipState Float
defense f_a9Xj (PlayerShipState x1_a9Xk x2_a9Xl x3_a9Xm x4_a9Xn)
  = fmap
      (\ y1_a9Xo -> PlayerShipState x1_a9Xk x2_a9Xl y1_a9Xo x4_a9Xn)
      (f_a9Xj x3_a9Xm)
{-# INLINE defense #-}
health :: Lens' PlayerShipState (Meter Int)
health f_a9Xp (PlayerShipState x1_a9Xq x2_a9Xr x3_a9Xs x4_a9Xt)
  = fmap
      (\ y1_a9Xu -> PlayerShipState y1_a9Xu x2_a9Xr x3_a9Xs x4_a9Xt)
      (f_a9Xp x1_a9Xq)
{-# INLINE health #-}
pSprite :: Lens' PlayerShipState (PSpriteState' VelActuator)
pSprite f_a9Xv (PlayerShipState x1_a9Xw x2_a9Xx x3_a9Xy x4_a9Xz)
  = fmap
      (\ y1_a9XA -> PlayerShipState x1_a9Xw x2_a9Xx x3_a9Xy y1_a9XA)
      (f_a9Xv x4_a9Xz)
{-# INLINE pSprite #-}

-- the state definition of the GamePlay scene
data GamePlaySceneState m = GamePlaySceneState
    { -- a set of bullets to be reused by the ships
      _bulletPool :: P.Pool (BulletState m)
    , _playerShip :: PlayerShipState
    , _enemyShipPool :: P.Pool (PSpriteState' VelActuator)
    }
bulletPool ::
  forall m_a4Tx.
  Lens' (GamePlaySceneState m_a4Tx) (P.Pool (BulletState m_a4Tx))
bulletPool f_a7Q9 (GamePlaySceneState x1_a7Qa x2_a7Qb x3_a7Qc)
  = fmap
      (\ y1_a7Qd -> GamePlaySceneState y1_a7Qd x2_a7Qb x3_a7Qc)
      (f_a7Q9 x1_a7Qa)
{-# INLINE bulletPool #-}
enemyShipPool ::
  forall m_a4Tx.
  Lens' (GamePlaySceneState m_a4Tx) (P.Pool (PSpriteState' VelActuator))
enemyShipPool f_a7Qe (GamePlaySceneState x1_a7Qf x2_a7Qg x3_a7Qh)
  = fmap
      (\ y1_a7Qi -> GamePlaySceneState x1_a7Qf x2_a7Qg y1_a7Qi)
      (f_a7Qe x3_a7Qh)
{-# INLINE enemyShipPool #-}
playerShip ::
  forall m_a4Tx. Lens' (GamePlaySceneState m_a4Tx) PlayerShipState
playerShip f_a7Qj (GamePlaySceneState x1_a7Qk x2_a7Ql x3_a7Qm)
  = fmap
      (\ y1_a7Qn -> GamePlaySceneState x1_a7Qk y1_a7Qn x3_a7Qm)
      (f_a7Qj x2_a7Ql)
{-# INLINE playerShip #-}

initPlayerShipState :: V2 Float -> PlayerShipState
initPlayerShipState winSize = PlayerShipState
    { _health  = Meter 100 100
    , _attack  = 20
    , _defense = 30
    , _pSprite = def & actuator.pos .~ playerStartPos
                     & sCategory    .~ PlayerShip
                     & sGeometry    .~ Polygon (reverse $ uncurry rect $ unr2 playerShipContour)
                     & sMass        .~ 5000
                     & sprName      .~ "res/img/player.png"
                     & enabled      .~ True
    }
  where playerStartPos = 0 .+^ (winSize & _x .~ 200
                                      & _y //~ 2)
        playerShipContour = 140^&40

initBulletPool :: Monad m => P.Pool (BulletState m)
initBulletPool = P.fromList $ replicate 25 def

initGamePlaySceneState :: Monad m
                   => V2 Float -- ^ Win size
                   -> GamePlaySceneState m
initGamePlaySceneState winSize = GamePlaySceneState
    { _bulletPool = initBulletPool
    , _playerShip = initPlayerShipState winSize
    , _enemyShipPool = P.fromList $ replicate 10 def
    }

-- | create a standard round bullet PSpriteState with the initial position, velocity and
-- acceleration
bullet :: Monad m => CollisionCategory -> P2 Float -> V2 Float -> V2 Float -> BulletState m
bullet ct p v acc = def & actuator  .~ AnyActuator accelAct
                        & sCategory .~ ct
                        & sGeometry .~ Polygon (reverse $ square 5)
                        & sMass     .~ 5
                        & sprName   .~ "res/img/bullet.png"
                        & enabled   .~ True
    where accelAct :: AccelActuator
          accelAct = def & pos .~ p
                         & vel .~ v
                         & accel .~ acc

