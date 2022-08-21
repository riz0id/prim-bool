{-# LANGUAGE UnboxedTuples #-}
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
    fromBool#,
    toBool#,
    fromInt#,
    toInt#,
    fromWord#,
    toWord#,

    -- ** String Casts #bool-string-casts#
    read#,
    unsafeRead#,
    show#,
  )
where

import GHC.Exts (Int#, Word#)
import qualified GHC.Exts as GHC

--------------------------------------------------------------------------------

import Data.Bool.Prim.Core (Bool# (Bool#, False#, True#))

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
-- | 'Int#'    | 'fromInt#'     | 'toInt#'       |   *   |
-- +-----------+----------------+----------------+-------+
-- | 'Word#'   | 'fromWord#'    | 'toWord#'      |   *   |
-- +-----------+----------------+----------------+-------+

-- Casts - Bool Casts ----------------------------------------------------------

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
toBool# (Bool# x) = GHC.tagToEnum# x

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

-- Casts - Read & Show ---------------------------------------------------------

-- | Parse a 'String' to a 'Bool#' value.
--
-- @since 1.0.0
read# :: String -> (# Bool#| (# #) #)
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