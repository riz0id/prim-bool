{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TemplateHaskellQuotes #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE UnliftedNewtypes #-}
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
  ( -- * Bool# #bool#
    Bool# (Bool#, False#, True#),

    -- * Logical #bool-logical#
    -- $section-bool-logical
    and#,
    or#,
    xor#,
    not#,

    -- * Comparison #bool-comparison#
    -- $section-bool-comparison
    gt#,
    ge#,
    eq#,
    ne#,
    lt#,
    le#,

    -- * Casts #bool-casts#
    -- $section-bool-casts

    -- ** Bool Casts #bool-bool-casts#
    -- $section-bool-bool-casts
    fromBool#,
    toBool#,

    -- ** String Casts #bool-string-casts#
    -- $section-bool-string-casts
    read#,
    unsafeRead#,
    show#,

    -- ** Char Casts #bool-char-casts#
    -- $section-bool-char-casts
    fromChar#,
    toChar#,

    -- ** Int Casts #bool-int-casts#
    -- $section-bool-int-casts
    fromInt#,
    toInt#,
    fromInt8#,
    toInt8#,
    fromInt16#,
    toInt16#,
#if (MIN_VERSION_ghc_prim(0,8,0))
    fromInt32#,
    toInt32#,
#endif

    -- ** Word Casts #bool-word-casts#
    -- $section-bool-word-casts
    fromWord#,
    toWord#,
    fromWord8#,
    toWord8#,
    fromWord16#,
    toWord16#,
#if (MIN_VERSION_ghc_prim(0,8,0))
    fromWord32#,
    toWord32#,
#endif

    -- ** Floating-Point Casts #bool-floating-point-casts#
    -- $section-bool-floating-point-casts
    toFloat#,
    toDouble#,
  )
where

import qualified GHC.Exts as GHC (unsafeCoerce#)
import GHC.Int (Int (I#))
import GHC.Prim
  ( Char#,
    Double#,
    Float#,
    Int#,
    Int16#,
    Int8#,
    Word#,
    Word16#,
    Word8#,
  )
import qualified GHC.Prim as GHC

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
pattern False# = Bool# 0#

{-# COMPLETE True#, False# #-}

-- | @since 1.0.0
instance Lift Bool# where
  lift (Bool# x) = pure (AppE (ConE 'Bool#) (LitE (IntPrimL (fromIntegral (I# x)))))
  {-# INLINE CONLIKE lift #-}

  liftTyped x = unsafeCodeCoerce (lift x)
  {-# INLINE CONLIKE liftTyped #-}

-- Bool# - Logical -------------------------------------------------------------

-- $section-bool-logical #section-bool-logical#
--
-- Logical operations on 'Bool#'.

infixl 5 `or#`
infixl 6 `xor#`
infixl 7 `and#`

-- | Boolean "and"
--
-- @since 1.0.0
and# :: Bool# -> Bool# -> Bool#
and# (Bool# x) (Bool# y) = Bool# (GHC.andI# x y)

-- | Boolean "or"
--
-- @since 1.0.0
or# :: Bool# -> Bool# -> Bool#
or# (Bool# x) (Bool# y) = Bool# (GHC.orI# x y)

-- | Boolean "exclusive or"
--
-- @since 1.0.0
xor# :: Bool# -> Bool# -> Bool#
xor# (Bool# x) (Bool# y) = Bool# (x GHC./=# y)

-- | Boolean "not"
--
-- @since 1.0.0
not# :: Bool# -> Bool#
not# True# = False# 
not# _ = True#

-- Bool# - Comparison ----------------------------------------------------------

-- $section-bool-comparison #section-bool-comparison#
--
-- Comparison on 'Bool#' values.

infix 4 `gt#`, `ge#`, `eq#`, `ne#`, `lt#`, `le#`

-- | "Greater than" comparison on two 'Bool#' values.
--
-- @since 1.0.0
gt# :: Bool# -> Bool# -> Bool#
gt# (Bool# x) (Bool# y) = Bool# (x GHC.># y)

-- | "Greater than or equal to" comparison on two 'Bool#' values.
--
-- @since 1.0.0
ge# :: Bool# -> Bool# -> Bool#
ge# (Bool# x) (Bool# y) = Bool# (x GHC.>=# y)

-- | "Equal to" comparison on two 'Bool#' values.
--
-- @since 1.0.0
eq# :: Bool# -> Bool# -> Bool#
eq# (Bool# x) (Bool# y) = Bool# (x GHC.==# y)

-- | "Not equal to" comparison on two 'Bool#' values.
--
-- @since 1.0.0
ne# :: Bool# -> Bool# -> Bool#
ne# (Bool# x) (Bool# y) = Bool# (x GHC./=# y)

-- | "Less than" comparison on two 'Bool#' values.
--
-- @since 1.0.0
lt# :: Bool# -> Bool# -> Bool#
lt# (Bool# x) (Bool# y) = Bool# (x GHC.<# y)

-- | "Less than or equal to" comparison on two 'Bool#' values.
--
-- @since 1.0.0
le# :: Bool# -> Bool# -> Bool#
le# (Bool# x) (Bool# y) = Bool# (x GHC.<=# y)

-- Casts -----------------------------------------------------------------------

-- $section-bool-casts #section-bool-casts#
--
-- Casts between 'Bool#' (unboxed booleans), 'Bool' (boxed booleans), 'String',
-- and other primitive unboxed values. All conversion functions are provided
-- in pairs with the naming convention:
--
-- * @fromX#@ to indicate a conversion from type @X@ to 'Bool#'.
--
-- * @toX#@ to indicate a conversion from 'Bool#' to a type @X@.
--
-- == Bool# Conversion Table
--
-- Below is the reference conversion table of with all supported 'Bool#'
-- conversions. Lossy conversions do not necessarily perserve the original value
-- provided to a conversion function through a round-trip.
--
-- +-----------+----------------+----------------+-------+
-- | Type      | @X -> 'Bool#'@ | @'Bool#' -> X@ | Lossy |
-- +===========+================+================+=======+
-- | 'Bool'    | 'fromBool#'    | 'toBool#'      |       |
-- +-----------+----------------+----------------+-------+
-- | 'String'  | 'read#'        | 'show#'        |       |
-- +-----------+----------------+----------------+-------+
-- | 'Char#'   | 'fromChar#'    | 'toChar#'      |   *   |
-- +-----------+----------------+----------------+-------+
-- | 'Int#'    | 'fromInt#'     | 'toInt#'       |   *   |
-- +-----------+----------------+----------------+-------+
-- | 'Int8#'   | 'fromInt8#'    | 'toInt8#'      |   *   |
-- +-----------+----------------+----------------+-------+
-- | 'Int16#'  | 'fromInt16#'   | 'toInt16#'     |   *   |
-- +-----------+----------------+----------------+-------+
-- | 'Int32#'  | 'fromInt32#'   | 'toInt32#'     |   *   |
-- +-----------+----------------+----------------+-------+
-- | 'Word#'   | 'fromWord#'    | 'toWord#'      |   *   |
-- +-----------+----------------+----------------+-------+
-- | 'Word8#'  | 'fromWord8#'   | 'toWord8#'     |   *   |
-- +-----------+----------------+----------------+-------+
-- | 'Word16#' | 'fromInt16#'   | 'toWord16#'    |   *   |
-- +-----------+----------------+----------------+-------+
-- | 'Word32#' | 'fromInt32#'   | 'toWord32#'    |   *   |
-- +-----------+----------------+----------------+-------+
-- | 'Float#'  |                | 'toFloat#'     |  N/A  |
-- +-----------+----------------+----------------+-------+
-- | 'Double#' |                | 'toDouble#'    |  N/A  |
-- +-----------+----------------+----------------+-------+

-- Casts - Bool Casts ----------------------------------------------------------

-- $section-bool-bool-casts #section-bool-bool-casts#
--
-- Conversion between 'Bool#' (unboxed boolean) values and 'Bool' (boxed
-- boolean) values.

-- | Converts a 'Bool' (boxed boolean) value to an equivalent 'Bool#' (unboxed
-- boolean) value.
--
-- @since 1.0.0
fromBool# :: Bool -> Bool#
fromBool# True = True#
fromBool# False = False#

-- | Converts an 'Bool#' (unboxed boolean) value to an equivalent 'Bool' (boxed
-- boolean) value.
--
-- @since 1.0.0
toBool# :: Bool# -> Bool
toBool# True# = True
toBool# _ = False

-- Casts - String Casts --------------------------------------------------------

-- $section-bool-string-casts #section-bool-string-casts#
--
-- Conversion between 'Bool#' values and 'String'.

-- | Parse a 'String' to a 'Bool#' value.
--
-- @since 1.0.0
read# :: String -> (# Bool# | (# #) #)
read# "True#" = (# True# | #)
read# "False#" = (# False# | #)
read# _ = (# | (##) #)

-- | Like 'read#', but will fail with an unchecked exception if the given 
-- 'String' is not "True#" or "False#".
--
-- @since 1.0.0
unsafeRead# :: String -> Bool#
unsafeRead# "True#" = True#
unsafeRead# "False#" = False#
unsafeRead# _ = errorWithoutStackTrace "Data.Int.Prim.unsafeRead#: failed to parse Bool#"

-- | Displays a 'Bool#' value as a 'String'.
--
-- @since 1.0.0
show# :: Bool# -> String
show# True# = "True#"
show# _ = "False#"

-- Casts - Char Casts ----------------------------------------------------------

-- $section-bool-char-casts #section-bool-char-casts#
--
-- Conversion between 'Bool#' values and 'Char#' (unboxed character values).

-- | Converts an 'Char#' to an unboxed boolean.
--
-- @since 1.0.0
fromChar# :: Char# -> Bool#
fromChar# x = Bool# (GHC.eqChar# (GHC.unsafeCoerce# 1#) x)

-- | Converts an unboxed boolean to an 'Char#'.
--
-- @since 1.0.0
toChar# :: Bool# -> Char#
toChar# (Bool# x) = GHC.unsafeCoerce# x

-- Casts - Int Casts -----------------------------------------------------------

-- $section-bool-int-casts #section-bool-int-casts#
--
-- Conversion between 'Bool#' values and values of the unboxed integer types:
-- 'Int#', 'Int8#', 'Int16#', and 'Int32#'.

-- | Converts an 'Int#' to an unboxed boolean.
--
-- @since 1.0.0
fromInt# :: Int# -> Bool#
fromInt# x = Bool# (1# GHC.==# x)

-- | Converts an unboxed boolean to an 'Int#'.
--
-- @since 1.0.0
toInt# :: Bool# -> Int#
toInt# (Bool# x) = x

-- | Converts an 'Int8#' to an unboxed boolean.
--
-- @since 1.0.0
fromInt8# :: Int8# -> Bool#
fromInt8# x = Bool# (GHC.eqInt8# (GHC.unsafeCoerce# 1#) x)

-- | Converts an unboxed boolean to an 'Int8#'.
--
-- @since 1.0.0
toInt8# :: Bool# -> Int8#
toInt8# (Bool# x) = GHC.unsafeCoerce# x

-- | Converts an 'Int16#' to an unboxed boolean.
--
-- @since 1.0.0
fromInt16# :: Int16# -> Bool#
fromInt16# x = Bool# (GHC.eqInt16# (GHC.unsafeCoerce# 1#) x)

-- | Converts an unboxed boolean to an 'Int16#'.
--
-- @since 1.0.0
toInt16# :: Bool# -> Int16#
toInt16# (Bool# x) = GHC.unsafeCoerce# x

#if (MIN_VERSION_ghc_prim(0,8,0))

-- | Converts an 'Int32#' to an unboxed boolean.
--
-- @since 1.0.0
fromInt32# :: Int32# -> Bool#
fromInt32# x = Bool# (GHC.eqInt32# (GHC.unsafeCoerce# 1#) x)

-- | Converts an unboxed boolean to an 'Int32#'.
--
-- @since 1.0.0
toInt32# :: Bool# -> Int32#
toInt32# (Bool# x) = GHC.unsafeCoerce# x

#endif

-- Casts - Word Casts ----------------------------------------------------------

-- $section-bool-word-casts #section-bool-word-casts#
--
-- Conversion between 'Bool#' values and values of the unboxed unsigned integer
-- types: 'Word#', 'Word8#', 'Word16#', and 'Word32#'.

-- | Converts an 'Word#' to an unboxed boolean.
--
-- @since 1.0.0
fromWord# :: Word# -> Bool#
fromWord# x = Bool# (GHC.eqWord# 1## x)

-- | Converts an unboxed boolean to an 'Word#'.
--
-- @since 1.0.0
toWord# :: Bool# -> Word#
toWord# (Bool# x) = GHC.int2Word# x

-- | Converts an 'Word8#' to an unboxed boolean.
--
-- @since 1.0.0
fromWord8# :: Word8# -> Bool#
fromWord8# x = Bool# (GHC.eqWord8# (GHC.unsafeCoerce# 1##) x)

-- | Converts an unboxed boolean to an 'Word8#'.
--
-- @since 1.0.0
toWord8# :: Bool# -> Word8#
toWord8# x = GHC.unsafeCoerce# (toWord# x)

-- | Converts an 'Word16#' to an unboxed boolean.
--
-- @since 1.0.0
fromWord16# :: Word16# -> Bool#
fromWord16# x = Bool# (GHC.eqWord16# (GHC.unsafeCoerce# 1##) x)

-- | Converts an unboxed boolean to an 'Word16#'.
--
-- @since 1.0.0
toWord16# :: Bool# -> Word16#
toWord16# x = GHC.unsafeCoerce# (toWord# x)

#if (MIN_VERSION_ghc_prim(0,8,0))

-- | Converts an 'Word32#' to an unboxed boolean.
--
-- @since 1.0.0
fromWord32# :: Word32# -> Bool#
fromWord32# x = Bool# (Compat.eqWord32# (GHC.unsafeCoerce# 1##) x)

-- | Converts an unboxed boolean to an 'Word32#'.
--
-- @since 1.0.0
toWord32# :: Bool# -> Word32#
toWord32# x = GHC.unsafeCoerce# (toWord# x)

#endif

-- Casts - Floating Point Casts ------------------------------------------------

-- $section-bool-floating-point-casts #section-bool-floating-point-casts#
--
-- Conversion between 'Bool#' values to values of the unboxed floating-point
-- number types 'Float#' and 'Double#'.

-- | Converts an unboxed boolean to an 'Float#'.
--
-- @since 1.0.0
toFloat# :: Bool# -> Float#
toFloat# (Bool# x) = GHC.int2Float# x

-- | Converts an unboxed boolean to an 'Double#'.
--
-- @since 1.0.0
toDouble# :: Bool# -> Double#
toDouble# (Bool# x) = GHC.int2Double# x