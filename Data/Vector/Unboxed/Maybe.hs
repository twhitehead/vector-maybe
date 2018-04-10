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
-- Adaptive unboxed vectors: add Maybe instance based on the (a,b) 2-tuple instance.
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

data instance U.MVector s (Maybe a) = MV_Maybe {-# UNPACK #-} !Int !(U.MVector s Bool) !(U.MVector s a)
data instance U.Vector    (Maybe a) = V_Maybe  {-# UNPACK #-} !Int !(U.Vector    Bool) !(U.Vector    a)

instance (U.Unbox a) => U.Unbox (Maybe a)


-- Standard `M.MVector` and `G.Vector` boilerplate, mostly lifted from the @(a,b)@ 2-tuple instance.
--
-- The 2-tuple code is found under /internal\/unbox-tuple-instances/ in the vector source tree.  As little was
-- changed as possible (@(a,b)@ to @(Bool,b)@ and the read/write functions) to make it easy to sync.

instance (U.Unbox b) => M.MVector U.MVector (Maybe b) where
  {-# INLINE basicLength  #-}
  basicLength (MV_Maybe n_ _ _) = n_
  {-# INLINE basicUnsafeSlice  #-}
  basicUnsafeSlice i_ m_ (MV_Maybe _ as bs)
      = MV_Maybe m_ (M.basicUnsafeSlice i_ m_ as)
                    (M.basicUnsafeSlice i_ m_ bs)
  {-# INLINE basicOverlaps  #-}
  basicOverlaps (MV_Maybe _ as1 bs1) (MV_Maybe _ as2 bs2)
      = M.basicOverlaps as1 as2
        || M.basicOverlaps bs1 bs2
  {-# INLINE basicUnsafeNew  #-}
  basicUnsafeNew n_
      = do
          as <- M.basicUnsafeNew n_
          bs <- M.basicUnsafeNew n_
          return $ MV_Maybe n_ as bs
  {-# INLINE basicInitialize  #-}
  basicInitialize (MV_Maybe _ as bs)
      = do
          M.basicInitialize as
          M.basicInitialize bs
  {-# INLINE basicUnsafeReplicate  #-}
  basicUnsafeReplicate n_ (Just b)
      = do
          as <- M.basicUnsafeReplicate n_ True
          bs <- M.basicUnsafeReplicate n_ b
          return $ MV_Maybe n_ as bs
  basicUnsafeReplicate n_ Nothing
      = do
          as <- M.basicUnsafeReplicate n_ False
          bs <- M.basicUnsafeNew       n_
          return $ MV_Maybe n_ as bs
  {-# INLINE basicUnsafeRead  #-}
  basicUnsafeRead (MV_Maybe _ as bs) i_
      = do
          a <- M.basicUnsafeRead as i_
          if a
            then do
              b <- M.basicUnsafeRead bs i_
              return (Just b)
            else do
              return Nothing
  {-# INLINE basicUnsafeWrite  #-}
  basicUnsafeWrite (MV_Maybe _ as bs) i_ (Just b)
      = do
          M.basicUnsafeWrite as i_ True
          M.basicUnsafeWrite bs i_ b
  basicUnsafeWrite (MV_Maybe _ as bs) i_ Nothing
      = do
          M.basicUnsafeWrite as i_ False
  {-# INLINE basicClear  #-}
  basicClear (MV_Maybe _ as bs)
      = do
          M.basicClear as
          M.basicClear bs
  {-# INLINE basicSet  #-}
  basicSet (MV_Maybe _ as bs) (Just b)
      = do
          M.basicSet as True
          M.basicSet bs b
  basicSet (MV_Maybe _ as bs) Nothing
      = do
          M.basicSet as False
  {-# INLINE basicUnsafeCopy  #-}
  basicUnsafeCopy (MV_Maybe _ as1 bs1) (MV_Maybe _ as2 bs2)
      = do
          M.basicUnsafeCopy as1 as2
          M.basicUnsafeCopy bs1 bs2
  {-# INLINE basicUnsafeMove  #-}
  basicUnsafeMove (MV_Maybe _ as1 bs1) (MV_Maybe _ as2 bs2)
      = do
          M.basicUnsafeMove as1 as2
          M.basicUnsafeMove bs1 bs2
  {-# INLINE basicUnsafeGrow  #-}
  basicUnsafeGrow (MV_Maybe n_ as bs) m_
      = do
          as' <- M.basicUnsafeGrow as m_
          bs' <- M.basicUnsafeGrow bs m_
          return $ MV_Maybe (m_+n_) as' bs'

instance (U.Unbox b) => G.Vector U.Vector (Maybe b) where
  {-# INLINE basicUnsafeFreeze  #-}
  basicUnsafeFreeze (MV_Maybe n_ as bs)
      = do
          as' <- G.basicUnsafeFreeze as
          bs' <- G.basicUnsafeFreeze bs
          return $ V_Maybe n_ as' bs'
  {-# INLINE basicUnsafeThaw  #-}
  basicUnsafeThaw (V_Maybe n_ as bs)
      = do
          as' <- G.basicUnsafeThaw as
          bs' <- G.basicUnsafeThaw bs
          return $ MV_Maybe n_ as' bs'
  {-# INLINE basicLength  #-}
  basicLength (V_Maybe n_ _ _) = n_
  {-# INLINE basicUnsafeSlice  #-}
  basicUnsafeSlice i_ m_ (V_Maybe _ as bs)
      = V_Maybe m_ (G.basicUnsafeSlice i_ m_ as)
               (G.basicUnsafeSlice i_ m_ bs)
  {-# INLINE basicUnsafeIndexM  #-}
  basicUnsafeIndexM (V_Maybe _ as bs) i_
      = do
          a <- G.basicUnsafeIndexM as i_
          if a
            then do
              b <- G.basicUnsafeIndexM bs i_
              return (Just b)
            else do
              return Nothing
  {-# INLINE basicUnsafeCopy  #-}
  basicUnsafeCopy (MV_Maybe _ as1 bs1) (V_Maybe _ as2 bs2)
      = do
          G.basicUnsafeCopy as1 as2
          G.basicUnsafeCopy bs1 bs2
  {-# INLINE elemseq  #-}
  elemseq _ (Just b ) z
      = G.elemseq (undefined :: U.Vector b) b z
  elemseq _ (Nothing) z
      = z
