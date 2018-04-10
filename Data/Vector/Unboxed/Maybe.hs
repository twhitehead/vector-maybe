{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
-- |
-- Module      : Data.Vector.Unboxed.Maybe
-- Description : Unboxed Maybe vectors
-- Copyright   : (c) Tyson Whitehead 2018
-- License     : BSD-style
--
-- Maintainer  : twhitehead@gmail.com
-- Portability : non-portable
--
-- Adaptive unboxed vectors: add Maybe instance based on Complex and Bool
--

module Data.Vector.Unboxed.Maybe
  ( module U
  ) where

import qualified Data.Vector.Unboxed         as U
import qualified Data.Vector.Generic         as G
import qualified Data.Vector.Generic.Mutable as M

import           Data.Maybe ( Maybe(Just,Nothing) )

import           Control.Monad    ( liftM )
import           Control.Monad.ST ( runST, ST )


-- -------
-- Maybe a
-- -------

-- $ Maybe a
--
-- An unboxed @Maybe a@ vector is internally just a composition of the unboxed @(Bool,a)@ vector where the `Bool`
-- value is used to mask the @a@ value.
--
-- The unboxed `Bool` vector is currently just a unboxed `Word8` vector internally.  This uses 8x more memory for
-- the mask then technically required, so a future option would be to switch to using the `Data.Bit` from bitvec.

newtype instance U.MVector s (Maybe a) = MV_Maybe (U.MVector s (Bool,a))
newtype instance U.Vector    (Maybe a) = V_Maybe  (U.Vector    (Bool,a))

instance (U.Unbox a) => U.Unbox (Maybe a)


-- | `initialized` provides a value for @a@ when tranlating @Nothing@ to @(False,a)@.
--
-- Use the default @Unbox a@ initialized value.  Digging through the code reveals this is just whatever you cast
-- from a memory that is all zeros (for most integer and float encodings this equals 0).

initialized :: forall a. U.Unbox a => a
initialized = runST $ do
  vector <- M.new 1 :: forall s. ST s (U.MVector s a)
  value  <- M.read vector 1
  return value


-- | `fromMaybe` translate from @Maybe a@ to @(Bool,a)@ using `initialzed` for masked @a@ values.

fromMaybe :: U.Unbox a => Maybe a -> (Bool,a)
{-# INLINE fromMaybe #-}
fromMaybe (Just a) = (True ,a)
fromMaybe Nothing  = (False,initialized)


-- | `toMaybe` translate from @(Bool,a)@ to @Maybe a@ by treating the @Bool@ as a mask.

toMaybe :: (Bool,a) -> Maybe a
toMaybe (True ,a) = Just a
toMaybe (False,_) = Nothing
{-# INLINE toMaybe #-}


-- Standard `M.MVector` and `G.Vector` boilerplate, mostly lifted from `Complex` and `Bool` instances.

instance (U.Unbox a) => M.MVector U.MVector (Maybe a) where
  {-# INLINE basicLength #-}
  {-# INLINE basicUnsafeSlice #-}
  {-# INLINE basicOverlaps #-}
  {-# INLINE basicUnsafeNew #-}
  {-# INLINE basicInitialize #-}
  {-# INLINE basicUnsafeReplicate #-}
  {-# INLINE basicUnsafeRead #-}
  {-# INLINE basicUnsafeWrite #-}
  {-# INLINE basicClear #-}
  {-# INLINE basicSet #-}
  {-# INLINE basicUnsafeCopy #-}
  {-# INLINE basicUnsafeGrow #-}
  basicLength (MV_Maybe v) = M.basicLength v
  basicUnsafeSlice i n (MV_Maybe v) = MV_Maybe $ M.basicUnsafeSlice i n v
  basicOverlaps (MV_Maybe v1) (MV_Maybe v2) = M.basicOverlaps v1 v2
  basicUnsafeNew n = MV_Maybe `liftM` M.basicUnsafeNew n
  basicInitialize (MV_Maybe v) = M.basicInitialize v
  basicUnsafeReplicate n m = MV_Maybe `liftM` M.basicUnsafeReplicate n (fromMaybe m)
  basicUnsafeRead (MV_Maybe v) i = toMaybe `liftM` M.basicUnsafeRead v i
  basicUnsafeWrite (MV_Maybe v) i m = M.basicUnsafeWrite v i (fromMaybe m)
  basicClear (MV_Maybe v) = M.basicClear v
  basicSet (MV_Maybe v) m = M.basicSet v (fromMaybe m)
  basicUnsafeCopy (MV_Maybe v1) (MV_Maybe v2) = M.basicUnsafeCopy v1 v2
  basicUnsafeMove (MV_Maybe v1) (MV_Maybe v2) = M.basicUnsafeMove v1 v2
  basicUnsafeGrow (MV_Maybe v) n = MV_Maybe `liftM` M.basicUnsafeGrow v n

instance (U.Unbox a) => G.Vector U.Vector (Maybe a) where
  {-# INLINE basicUnsafeFreeze #-}
  {-# INLINE basicUnsafeThaw #-}
  {-# INLINE basicLength #-}
  {-# INLINE basicUnsafeSlice #-}
  {-# INLINE basicUnsafeIndexM #-}
  {-# INLINE elemseq #-}
  basicUnsafeFreeze (MV_Maybe v) = V_Maybe `liftM` G.basicUnsafeFreeze v
  basicUnsafeThaw (V_Maybe v) = MV_Maybe `liftM` G.basicUnsafeThaw v
  basicLength (V_Maybe v) = G.basicLength v
  basicUnsafeSlice i n (V_Maybe v) = V_Maybe $ G.basicUnsafeSlice i n v
  basicUnsafeIndexM (V_Maybe v) i = toMaybe `liftM` G.basicUnsafeIndexM v i
  basicUnsafeCopy (MV_Maybe mv) (V_Maybe v) = G.basicUnsafeCopy mv v
  elemseq _ (Just a) z = G.elemseq (undefined :: U.Vector a) a z
  elemseq _ Nothing   z = z
