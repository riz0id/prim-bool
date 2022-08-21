{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE TemplateHaskellQuotes #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE UnliftedNewtypes #-}
{-# OPTIONS_HADDOCK show-extensions #-}

-- |
-- Module      :  Data.Bool.Prim.Core
-- Copyright   :  (c) Jacob Leach, 2022
-- License     :  ISC, see LICENSE
--
-- Maintainer  :  jacobleach@protonmail.com
-- Stability   :  stable
-- Portability :  non-portable (GHC extensions)
--
-- Unboxed booleans 'Bool#' and wrapped 'GHC.Prim' operations
--
-- @since 1.0.0
module Data.Bool.Prim.Core
  ( -- * Bool#
    Bool# (Bool#, False#, True#),
  )
where

import GHC.Exts (Int (I#), Int#)

import Language.Haskell.TH.Syntax
  ( Exp (AppE, ConE, LitE),
    Lift (lift, liftTyped),
    Lit (IntPrimL),
    unsafeCodeCoerce,
  )

-- Bool# -----------------------------------------------------------------------

-- | 'Bool#' is an unboxed boolean type, analogous to 'Bool'. A 'Bool#' value is
-- represented by an 'Int#' that is either 'True#' or 'False#'.
--
-- @since 1.0.0
newtype Bool# = Bool# Int#

-- | Like 'True', but for 'Bool#'.
--
-- @since 1.0.0
pattern True# :: Bool#
pattern True# = Bool# 1#

-- | Like 'False', but for 'Bool#'.
--
-- @since 1.0.0
pattern False# :: Bool#
pattern False# <- ((\_ -> Bool# 0#) -> Bool# 0#)
  where False# = Bool# 0#

{-# COMPLETE True#, False# #-}

-- | @since 1.0.0
instance Lift Bool# where
  lift (Bool# x) = pure (AppE (ConE 'Bool#) (LitE (IntPrimL (fromIntegral (I# x)))))
  {-# INLINE CONLIKE lift #-}

  liftTyped x = unsafeCodeCoerce (lift x)
  {-# INLINE CONLIKE liftTyped #-}