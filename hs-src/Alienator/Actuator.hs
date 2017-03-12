{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
-- | varieties of actuating components (to drive Transform)
module Alienator.Actuator
  (
    IsActuator(..)
  , AnyActuator(..)
  , VelActuator(VelActuator)
  , HasVel(..)
  , HasPos(..)
  , HasFollowRotation(..)
  , AccelActuator(AccelActuator)
  , HasAccel(..)
  , Anchor(Anchor)
  , anchorPos
  ) where

import Data.Default
import Data.Typeable
import Diagrams
import Linear.Affine
import Reflex.Cocos2d
import Control.Lens

class IsActuator a where
    updateTick :: Time -> a -> a

newtype Anchor = Anchor { _anchorPos :: P2 Float } deriving (Typeable, Show, Eq)

anchorPos :: Lens' Anchor (P2 Float)
anchorPos f (Anchor pos) = Anchor <$> f pos

instance {-# OVERLAPPING #-} Monad m => HasROPositionAttrib Anchor m where
    roPosition = ROAttrib $ \(Anchor pos) -> return pos

instance Monad m => HasROAngleAttrib Anchor m where
    roAngle = ROAttrib . const $ return (0 @@ rad)

instance IsActuator Anchor where
    updateTick _ = id

instance Default Anchor where
    def = Anchor 0

-- wrapper datatype to provide an interface for IsActuator
data AnyActuator m = forall a.
    ( IsActuator a
    , HasROPositionAttrib a m, HasROAngleAttrib a m
    , Typeable a, Show a, Eq a ) => AnyActuator a

instance Show (AnyActuator m) where
    show (AnyActuator a) = "Actuator " ++ show a

instance Eq (AnyActuator m) where
    AnyActuator a == AnyActuator b =
      case cast b of
        Just b' -> a == b'
        Nothing -> False

instance Monad m => Default (AnyActuator m) where
    def = AnyActuator (def :: Anchor)

instance {-# OVERLAPPING #-} Monad m => HasROPositionAttrib (AnyActuator m) m where
    roPosition = ROAttrib $ \(AnyActuator act) -> get act roPosition

instance {-# OVERLAPPING #-} Monad m => HasROAngleAttrib (AnyActuator m) m where
    roAngle = ROAttrib $ \(AnyActuator act) -> get act roAngle

instance IsActuator (AnyActuator m) where
  updateTick dt (AnyActuator a) = AnyActuator $ updateTick dt a

-- positional change based on velocity
data VelActuator = VelActuator
                 { _velActuatorVel :: V2 Float
                 , _velActuatorPos :: P2 Float
                 , _velActuatorFollowRotation :: Bool
                 } deriving (Show, Read, Eq, Typeable)

class HasFollowRotation s a | s -> a where
  followRotation :: Lens' s a
instance HasFollowRotation VelActuator Bool where
  {-# INLINE followRotation #-}
  followRotation f_abaV (VelActuator x1_abaW x2_abaX x3_abaY)
    = fmap
        (\ y1_abaZ -> VelActuator x1_abaW x2_abaX y1_abaZ) (f_abaV x3_abaY)
class HasPos s a | s -> a where
  pos :: Lens' s a
instance HasPos VelActuator (P2 Float) where
  {-# INLINE pos #-}
  pos f_abb0 (VelActuator x1_abb1 x2_abb2 x3_abb3)
    = fmap
        (\ y1_abb4 -> VelActuator x1_abb1 y1_abb4 x3_abb3) (f_abb0 x2_abb2)
class HasVel s a | s -> a where
  vel :: Lens' s a
instance HasVel VelActuator (V2 Float) where
  {-# INLINE vel #-}
  vel f_abb5 (VelActuator x1_abb6 x2_abb7 x3_abb8)
    = fmap
        (\ y1_abb9 -> VelActuator y1_abb9 x2_abb7 x3_abb8) (f_abb5 x1_abb6)

instance {-# OVERLAPPING #-} Monad m => HasROPositionAttrib VelActuator m where
    roPosition = ROAttrib $ \ac -> return (ac^.pos)

instance Monad m => HasROAngleAttrib VelActuator m where
    roAngle = ROAttrib $ \ac -> do
      let rot | ac^.followRotation = ac^.vel._theta
              | otherwise = 0 @@ rad
      return rot

instance IsActuator VelActuator where
    updateTick dt ac = ac & pos %~ (.+^ (realToFrac dt * ac^.vel))

instance Default VelActuator where
    def = VelActuator 0 0 False

data AccelActuator = AccelActuator
                   { _accelActuatorAccel :: V2 Float
                   , _accelActuatorVelActuator :: VelActuator
                   } deriving (Show, Read, Eq, Typeable)

class HasAccel s a | s -> a where
  accel :: Lens' s a
instance HasAccel AccelActuator (V2 Float) where
  {-# INLINE accel #-}
  accel f_abdK (AccelActuator x1_abdL x2_abdM)
    = fmap
        (\ y1_abdN -> AccelActuator y1_abdN x2_abdM) (f_abdK x1_abdL)
{-# INLINE velActuator #-}
velActuator :: Lens' AccelActuator VelActuator
velActuator f_abdO (AccelActuator x1_abdP x2_abdQ)
  = fmap
      (\ y1_abdR -> AccelActuator x1_abdP y1_abdR) (f_abdO x2_abdQ)

instance HasVel AccelActuator (V2 Float) where
    vel = velActuator . vel

instance HasPos AccelActuator (P2 Float) where
    pos = velActuator . pos

instance HasFollowRotation AccelActuator Bool where
    followRotation = velActuator . followRotation

instance {-# OVERLAPPING #-} Monad m => HasROPositionAttrib AccelActuator m where
    roPosition = ROAttrib $ flip get roPosition . _accelActuatorVelActuator

instance Monad m => HasROAngleAttrib AccelActuator m where
    roAngle = ROAttrib $ flip get roAngle . _accelActuatorVelActuator

instance IsActuator AccelActuator where
    updateTick dt ac = ac & velActuator %~ updateVelAct
      where updateVelAct = updateTick dt . (vel %~ (.+^ (realToFrac dt * ac^.accel)))

instance Default AccelActuator where
    def = AccelActuator 0 def

