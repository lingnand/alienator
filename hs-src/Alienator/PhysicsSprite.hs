{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Alienator.PhysicsSprite
  (
    PhysicsSpriteState(..)
  , HasActuator(..)
  , HasEnabled(..)
  , HasSprName(..)
  , HasSGeometry(..)
  , HasSMass(..)
  , HasSCategory(..)
  , physicsSprite
  ) where

import Data.Default
import Data.Functor.Misc
import Control.Lens
import Control.Monad
import Reflex
import Reflex.Host.Class
import Reflex.State
import Reflex.Cocos2d

import Alienator.Actuator

-- generalized sprite that supports physical interaction within DynSpace
data PhysicsSpriteState ac ct = PhysicsSpriteState
    { _physicsSpriteStateActuator  :: ac
    , _physicsSpriteStateSGeometry :: Geometry Float
    , _physicsSpriteStateSMass     :: Float
    , _physicsSpriteStateSCategory :: ct
    , _physicsSpriteStateSprName   :: String
    , _physicsSpriteStateEnabled   :: Bool
    } deriving (Show, Eq)

instance (Enum ct, Default ac) => Default (PhysicsSpriteState ac ct)  where
    def = PhysicsSpriteState
        { _physicsSpriteStateActuator = def
        , _physicsSpriteStateSGeometry = Polygon []
        , _physicsSpriteStateSMass = 100
        , _physicsSpriteStateSCategory = toEnum 0
        , _physicsSpriteStateSprName = ""
        , _physicsSpriteStateEnabled = False
        }

class HasActuator s a | s -> a where
  actuator :: Lens' s a
instance HasActuator (PhysicsSpriteState ac_a4JH ct_a4JI) ac_a4JH where
  {-# INLINE actuator #-}
  actuator
    f_aaev
    (PhysicsSpriteState x1_aaew
                        x2_aaex
                        x3_aaey
                        x4_aaez
                        x5_aaeA
                        x6_aaeB)
    = fmap
        (\ y1_aaeC
           -> PhysicsSpriteState
                y1_aaeC x2_aaex x3_aaey x4_aaez x5_aaeA x6_aaeB)
        (f_aaev x1_aaew)
class HasEnabled s a | s -> a where
  enabled :: Lens' s a
instance HasEnabled (PhysicsSpriteState ac_a4JH ct_a4JI) Bool where
  {-# INLINE enabled #-}
  enabled
    f_aaeD
    (PhysicsSpriteState x1_aaeE
                        x2_aaeF
                        x3_aaeG
                        x4_aaeH
                        x5_aaeI
                        x6_aaeJ)
    = fmap
        (\ y1_aaeK
           -> PhysicsSpriteState
                x1_aaeE x2_aaeF x3_aaeG x4_aaeH x5_aaeI y1_aaeK)
        (f_aaeD x6_aaeJ)
class HasSCategory s a | s -> a where
  sCategory :: Lens' s a
instance HasSCategory (PhysicsSpriteState ac_a4JH ct_a4JI) ct_a4JI where
  {-# INLINE sCategory #-}
  sCategory
    f_aaeL
    (PhysicsSpriteState x1_aaeM
                        x2_aaeN
                        x3_aaeO
                        x4_aaeP
                        x5_aaeQ
                        x6_aaeR)
    = fmap
        (\ y1_aaeS
           -> PhysicsSpriteState
                x1_aaeM x2_aaeN x3_aaeO y1_aaeS x5_aaeQ x6_aaeR)
        (f_aaeL x4_aaeP)
class HasSGeometry s a | s -> a where
  sGeometry :: Lens' s a
instance HasSGeometry (PhysicsSpriteState ac_a4JH ct_a4JI) (Geometry Float) where
  {-# INLINE sGeometry #-}
  sGeometry
    f_aaeT
    (PhysicsSpriteState x1_aaeU
                        x2_aaeV
                        x3_aaeW
                        x4_aaeX
                        x5_aaeY
                        x6_aaeZ)
    = fmap
        (\ y1_aaf0
           -> PhysicsSpriteState
                x1_aaeU y1_aaf0 x3_aaeW x4_aaeX x5_aaeY x6_aaeZ)
        (f_aaeT x2_aaeV)
class HasSMass s a | s -> a where
  sMass :: Lens' s a
instance HasSMass (PhysicsSpriteState ac_a4JH ct_a4JI) Float where
  {-# INLINE sMass #-}
  sMass
    f_aaf1
    (PhysicsSpriteState x1_aaf2
                        x2_aaf3
                        x3_aaf4
                        x4_aaf5
                        x5_aaf6
                        x6_aaf7)
    = fmap
        (\ y1_aaf8
           -> PhysicsSpriteState
                x1_aaf2 x2_aaf3 y1_aaf8 x4_aaf5 x5_aaf6 x6_aaf7)
        (f_aaf1 x3_aaf4)
class HasSprName s a | s -> a where
  sprName :: Lens' s a
instance HasSprName (PhysicsSpriteState ac_a4JH ct_a4JI) String where
  {-# INLINE sprName #-}
  sprName
    f_aaf9
    (PhysicsSpriteState x1_aafa
                        x2_aafb
                        x3_aafc
                        x4_aafd
                        x5_aafe
                        x6_aaff)
    = fmap
        (\ y1_aafg
           -> PhysicsSpriteState
                x1_aafa x2_aafb x3_aafc x4_aafd y1_aafg x6_aaff)
        (f_aaf9 x5_aafe)

physicsSprite :: ( NodeGraph t m, Maskable a, Eq a
                 , IsActuator ac, Eq ac
                 , HasROPositionAttrib ac (HostFrame t)
                 , HasROAngleAttrib ac (HostFrame t) )
              => Space a -- space
              -> Event t SpaceStep
              -> EventSelector t (Const2 (Body a) (ShapeAttributes a))
              -> DynStateT t (PhysicsSpriteState ac a) m (Event t a) -- collision events
physicsSprite sp steps collisionsE = do
    ticks <- view frameTicks
    sDyn <- watch
    let enabledDyn = view enabled <$> sDyn
        enabledBeh = current enabledDyn
        uniqEnabledDyn = uniqDyn enabledDyn
        uniqGeoDyn = uniqDyn $ view sGeometry <$> sDyn
    -- periodically update the actuator
    adjust $ over actuator . updateTick <$> gate enabledBeh ticks
    b <- body sp
          [ dyn transformFrom := uniqDyn (view actuator <$> sDyn)
          ]
    geo <- sample $ current uniqGeoDyn
    void $ shape sp b geo
      [ evt geometry  := updated uniqGeoDyn
      , dyn' active   := uniqEnabledDyn
      , dyn' category := uniqDyn (view sCategory <$> sDyn)
      , dyn' mass     := uniqDyn (view sMass <$> sDyn)
      ]
    sprite_
      [ transformFrom        := b
      , evt transformFrom    := b <$ steps
      , dyn' textureFilename := uniqDyn (view sprName <$> sDyn)
      , dyn' visible         := uniqEnabledDyn
      ]
    return $ view shapeCategory <$> select collisionsE (Const2 b)
