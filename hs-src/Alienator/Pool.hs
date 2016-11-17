{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE NamedFieldPuns #-}
-- A simple structure for a pool of resources
module Alienator.Pool
  (
    Pool
  , Id
  , fromList
  , empty
  , elems
  , idleIds
  , null
  , size
  , difference
  , foldrWithId
  , putNextIdle
  , putNextIdle'
  , putNextIdles
  , modifyIdle
  , markIdle
  , traverseWithId
  , traverseWithId_
  ) where

import Prelude hiding (null)
import qualified Data.IntMap.Strict as IM
import qualified Data.IntSet as IS
import Control.Lens

type Id = Int

data Pool a = Pool
           { _elems :: IM.IntMap a
           , _idleElems :: IS.IntSet
           } deriving (Show, Read, Eq)

elemsLens ::
  forall a_a4Ok a_ab7Y.
  Lens (Pool a_a4Ok) (Pool a_ab7Y) (IM.IntMap a_a4Ok) (IM.IntMap a_ab7Y)
elemsLens f_ab7Z (Pool x1_ab80 x2_ab81)
  = fmap (\ y1_ab82 -> Pool y1_ab82 x2_ab81) (f_ab7Z x1_ab80)
{-# INLINE elemsLens #-}

-- Lens for the mix
type instance Index (Pool a) = Id
type instance IxValue (Pool a) = a
instance Ixed (Pool a) where
    ix k = elemsLens . ix k

instance At (Pool a) where
    at k = elemsLens . at k

-- initiate a pool with all initial elements idle
fromList :: [a] -> Pool a
fromList ls = Pool (IM.fromList (zip inds ls)) (IS.fromList inds)
  where inds = zipWith const [0..] ls

empty :: Pool a
empty = Pool IM.empty IS.empty

elems :: Pool a -> [a]
elems = IM.elems . _elems

idleIds :: Pool a -> [Id]
idleIds = IS.elems . _idleElems

null :: Pool a -> Bool
null = IM.null . _elems

size :: Pool a -> Int
size = IM.size . _elems

difference :: Pool a -> Pool a -> Pool a
difference (Pool aElems aIdles) (Pool bElems _) = Pool (IM.difference aElems bElems)
                                                       (IS.difference aIdles (IM.keysSet bElems))

-- fold the ids and items in the pool
foldrWithId :: (Id -> a -> b -> b) -> b -> Pool a -> b
foldrWithId f b = IM.foldrWithKey f b . _elems

putNextIdle :: a -> Pool a -> Pool a
putNextIdle a = snd . putNextIdle' a

-- change the next idle element to the given state
-- if no idle element is available - insert a new element
putNextIdle' :: a -> Pool a -> (Id, Pool a)
putNextIdle' a p@Pool{ _elems, _idleElems }
  | IS.null _idleElems =
    let key = IM.size _elems
    in (key, p{ _elems = IM.insert key a _elems })
  | otherwise =
    let (key, idles') = IS.deleteFindMin _idleElems
    in (key, Pool{ _elems = IM.insert key a _elems, _idleElems = idles' })

putNextIdles :: [a] -> Pool a -> Pool a
putNextIdles = flip $ foldr putNextIdle

modifyIdle :: (a -> a) -> Pool a -> Pool a
modifyIdle f p@Pool{ _elems, _idleElems }
  | IS.null _idleElems = p
  | otherwise =
    let (key, idles') = IS.deleteFindMin _idleElems
    in Pool{ _elems = IM.adjust f key _elems, _idleElems = idles' }

markIdle :: Id -> Pool a -> Pool a
markIdle id p@Pool{ _elems, _idleElems }
  | IM.member id _elems = p{ _idleElems = IS.insert id _idleElems }
  | otherwise = p

traverseWithId :: Applicative f => (Id -> a -> f b) -> Pool a -> f [b]
traverseWithId f = foldrWithId (\pid a acc -> (:) <$> f pid a <*> acc) (pure [])

traverseWithId_ :: Applicative f => (Id -> a -> f ()) -> Pool a -> f ()
traverseWithId_ f = foldrWithId (\pid a acc -> f pid a *> acc) (pure ())

