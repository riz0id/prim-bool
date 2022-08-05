{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTSyntax #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE UnliftedNewtypes #-}
{-# LANGUAGE TemplateHaskellQuotes #-}
{-# OPTIONS_HADDOCK show-extensions #-}

-- |
-- Module      :  Data.Bool.Unlifted.Core
-- Copyright   :  (c) Jacob Leach, 2022
-- License     :  see LICENSE
--
-- Maintainer  :  jacobleach@protonmail.com
-- Stability   :  stable
-- Portability :  non-portable (GHC extensions)
--
-- TODO
--
-- @since 1.0.0
module Data.Bool.Unlifted.Core
  ( -- * TODO
    Bool# (B#, False#, True#),
  )
where

import Data.Typeable (Typeable)

import GHC.Prim (Int#)

--------------------------------------------------------------------------------

import Data.Bool.Unlifted.Rep (BoolType)

import GHC.Int (Int (I#))

import Language.Haskell.TH (Exp (AppE, ConE, LitE), Lit (IntPrimL))
import Language.Haskell.TH.Syntax (Lift (lift, liftTyped), unsafeCodeCoerce)

--------------------------------------------------------------------------------

-- | Unlifted boolean type, analogous to 'Bool'. A value of @Bool#@ represents 
-- an @Int#@ that is either 0# for 'False' or 1# for 'True'.
--
-- @since 1.0.0
newtype Bool# :: BoolType where
  B# :: Int# -> Bool#
  deriving Typeable

{-# COMPLETE True#, False# #-}

-- @since 1.0.0
instance Lift Bool# where
  lift (B# x) = pure (AppE (ConE 'B#) (LitE (IntPrimL (fromIntegral (I# x)))))
  {-# INLINE CONLIKE lift #-}

  liftTyped x = unsafeCodeCoerce (lift x)
  {-# INLINE CONLIKE liftTyped #-}

-- | Unlifted 'True' pattern synonym.
--
-- @since 1.0.0
pattern True# :: Bool#
pattern True# = B# 1#

-- | Unlifted 'False' pattern synonym.
--
-- @since 1.0.0
pattern False# :: Bool#
pattern False# = B# 0#