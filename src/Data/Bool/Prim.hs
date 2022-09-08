{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TemplateHaskellQuotes #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE UnliftedNewtypes #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_HADDOCK show-extensions #-}

-- |
-- Module      :  Data.Bool.Prim
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
module Data.Bool.Prim
  ( Bool# (False#, True#),

    -- * Logical
    and#,
    or#,
    xor#,
    not#,

    -- * Comparison
    gt#,
    ge#,
    eq#,
    ne#,
    lt#,
    le#,

    -- * Casts #bool-casts#
    fromBool,
    toBool,

    -- ** Integral Casts
    fromInt#,
    toInt#,
    fromWord#,
    toWord#,

    -- ** Unsafe
    unsafeFromInt#,

    -- * Read & Show
    read#,
    show#,

    -- ** Unsafe
    unsafeRead#,
  )
where

import GHC.Exts (Int (I#), Int#, Word#, coerce)
import GHC.Exts qualified as GHC

import Language.Haskell.TH.Syntax (Lift, lift, liftTyped, unsafeCodeCoerce)
import Language.Haskell.TH.Syntax qualified as TH

-- Bool# -----------------------------------------------------------------------

-- | 'Bool#' is an unboxed boolean type.
--
-- @since 1.0.0
newtype Bool# = Bool# Int#

-- | @since 1.0.0
instance Lift Bool# where
  lift (Bool# x#) = pure $ TH.AppE (TH.VarE 'Bool#) (TH.LitE $ TH.IntPrimL $ toInteger $ I# x#)
  {-# INLINE CONLIKE lift #-}

  liftTyped x = unsafeCodeCoerce (lift x)
  {-# INLINE CONLIKE liftTyped #-}

{-# COMPLETE True#, False# #-}

-- | Pattern synonym that matches to true 'Bool#' values.
--
-- @since 1.0.0
pattern True# :: Bool#
pattern True# = Bool# 1#

-- | Pattern synonym that matches to false 'Bool#' values.
--
-- @since 1.0.0
pattern False# :: Bool#
pattern False# <-
  ((\_ -> Bool# 0#) -> Bool# 0#)
  where
    False# = Bool# 0#

-- Bool# - Logical -------------------------------------------------------------

infixl 5 `or#`
infixl 6 `xor#`
infixl 7 `and#`

-- | Boolean "and"
--
-- @since 1.0.0
and# :: Bool# -> Bool# -> Bool#
and# = coerce GHC.andI#

-- | Boolean "or"
--
-- @since 1.0.0
or# :: Bool# -> Bool# -> Bool#
or# = coerce GHC.orI#

-- | Boolean "exclusive or"
--
-- @since 1.0.0
xor# :: Bool# -> Bool# -> Bool#
xor# = coerce (GHC./=#)

-- | Boolean "not"
--
-- @since 1.0.0
not# :: Bool# -> Bool#
not# True# = False#
not# False# = True#

-- Bool# - Comparison ----------------------------------------------------------

infix 4 `gt#`, `ge#`, `eq#`, `ne#`, `lt#`, `le#`

-- | "Greater than" comparison on two 'Bool#' values.
--
-- @since 1.0.1
gt# :: Bool# -> Bool# -> Bool#
gt# = coerce (GHC.>#)

-- | "Greater than or equal to" comparison on two 'Bool#' values.
--
-- @since 1.0.1
ge# :: Bool# -> Bool# -> Bool#
ge# = coerce (GHC.>=#)

-- | "Equal to" comparison on two 'Bool#' values.
--
-- @since 1.0.1
eq# :: Bool# -> Bool# -> Bool#
eq# = coerce (GHC.==#)

-- | "Not equal to" comparison on two 'Bool#' values.
--
-- @since 1.0.1
ne# :: Bool# -> Bool# -> Bool#
ne# = coerce (GHC./=#)

-- | "Less than" comparison on two 'Bool#' values.
--
-- @since 1.0.1
lt# :: Bool# -> Bool# -> Bool#
lt# = coerce (GHC.<#)

-- | "Less than or equal to" comparison on two 'Bool#' values.
--
-- @since 1.0.1
le# :: Bool# -> Bool# -> Bool#
le# = coerce (GHC.<=#)

-- Casts -----------------------------------------------------------------------

-- | Converts a 'Bool' (boxed boolean) value to an equivalent 'Bool#' (unboxed
-- boolean) value.
--
-- @since 1.0.0
fromBool :: Bool -> Bool#
fromBool = coerce (GHC.dataToTag# @Bool)

-- | Converts an 'Bool#' (unboxed boolean) value to an equivalent 'Bool' (boxed
-- boolean) value.
--
-- @since 1.0.0
toBool :: Bool# -> Bool
toBool x = GHC.tagToEnum# (coerce x)

-- Casts - Integral Casts ------------------------------------------------------

-- | Converts an 'Int#' to an unboxed boolean.
--
-- @since 1.0.0
fromInt# :: Int# -> Bool#
fromInt# 1# = True#
fromInt# _ = False#

-- | Converts an unboxed boolean to an 'Int#'.
--
-- @since 1.0.0
toInt# :: Bool# -> Int#
toInt# = coerce

-- | Converts an 'Word#' to an unboxed boolean.
--
-- @since 1.0.1
fromWord# :: Word# -> Bool#
fromWord# = coerce (GHC.eqWord# 1##)

-- | Converts an unboxed boolean to an 'Word#'.
--
-- @since 1.0.1
toWord# :: Bool# -> Word#
toWord# = coerce GHC.int2Word#

-- Casts - Unsafe Casts --------------------------------------------------------

-- | 'unsafeFromInt#' converts an 'Int#' known to be 0# or 1# to a 'Bool#'.
--
-- @since 1.0.1
unsafeFromInt# :: Int# -> Bool#
unsafeFromInt# = Bool# -- See note [Int# to Bool# Conversion]

-- Note [Int# to Bool# Conversion]
--
-- The implementation for fromInt# could be given one of a few ways:
--
--   * First, we could pattern match on the Int# argument and map it to the
--     equivalent Bool# value, that is:
--
--     @
--     fromInt# :: Int# -> Bool#
--     fromInt# 1# = True#
--     fromInt# _ = False#
--     @
--
--     This implementation is arguably the safest, but suffers from one major
--     drawback: it prevents prim-ops in Bool# functions from being inlined
--     properly due to case-expressions being interleaved between each
--     operations.
--
--   * Second, we could simply define fromInt# to be the raw Bool# constructor:
--
--     @
--     fromInt# :: Int# -> Bool
--     fromInt# = Bool#
--     @
--
--     This definition allow prim-ops to be inlined since matching on underlying
--     Int# of Bool# is deferred. This implementation is /highly/ unsafe
--     however. In many cases, using Bool# values obtained from the raw Bool#
--     constructor on arbitrary Int# inputs leads to unexpected results under
--     the Bool# logical operations.
--
-- The unsafe implementation of fromInt# is preferrable since the Bool#
-- constructor incurs no additional overhead over Int# as it is completely
-- erased in generated code. We cannot in good faith expose an unsafe function
-- under such an inconspicuous name.
--
-- Providing the safe version of fromInt# and exposing the constructor for Bool#
-- as a fallback seems irresponsible Bool#. For these reasons, unsafeFromInt#
-- and fromInt# are both provided in order balance safety and code generation
-- properties.

-- Read & Show -----------------------------------------------------------------

-- | Parse a 'String' to a 'Bool#' value.
--
-- @since 1.0.1
read# :: String -> (# Bool#| (# #) #)
read# "True#" = (# True# | #)
read# "False#" = (# False# | #)
read# _ = (# | (##) #)

-- | Like 'read#', but will fail with an unchecked exception if the given
-- 'String' is not "True#" or "False#".
--
-- @since 1.0.1
unsafeRead# :: String -> Bool#
unsafeRead# "True#" = True#
unsafeRead# "False#" = False#
unsafeRead# _ = errorWithoutStackTrace "Data.Int.Prim.unsafeRead#: failed to parse Bool#"

-- | Displays a 'Bool#' value as a 'String'.
--
-- @since 1.0.1
show# :: Bool# -> String
show# True# = "True#"
show# False# = "False#"