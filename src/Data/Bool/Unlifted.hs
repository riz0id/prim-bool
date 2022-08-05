{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MagicHash #-}
{-# OPTIONS_HADDOCK show-extensions #-}

-- |
-- Module      :  Data.Bool.Unlifted
-- Copyright   :  (c) Jacob Leach, 2022
-- License     :  see LICENSE
--
-- Maintainer  :  jacobleach@protonmail.com
-- Stability   :  stable
-- Portability :  non-portable (GHC extensions)
--
-- Unlifted booleans 'Bool#' and wrapped 'GHC.Prim' operations
--
-- @since 1.0.0
module Data.Bool.Unlifted
  ( -- * Unlifted Booleans
    Bool# (False#, True#),
    show#,

    -- * Logical Operations
    and#,
    or#,
    xor#,
    not#,

    -- * Comparison
    eq#,
    ne#,
    lt#,
    le#,
    gt#,
    ge#,

    -- * Conversion
    fromBool#,
    toBool#,

    -- ** Integer
    fromInt#,
    toInt#,
    fromInt8#,
    toInt8#,
    fromInt16#,
    toInt16#,
    fromInt32#,
    toInt32#,

    -- ** Word
    fromWord#,
    toWord#,
    fromWord8#,
    toWord8#,
    fromWord16#,
    toWord16#,
    fromWord32#,
    toWord32#,
  )
where

import GHC.Prim (Int#, Int16#, Int32#, Int8#, Word#, Word8#, Word16#, Word32#)
import qualified GHC.Prim as Prim

--------------------------------------------------------------------------------

import Data.Bool.Unlifted.Core (Bool# (B#, False#, True#))

-- Data.Bool.Unlifted.Core -----------------------------------------------------

-- | Displays an unlifted boolean value as a 'String'.
--
-- @since 1.0.0
show# :: Bool# -> String
show# True# = "True#"
show# False# = "False#"

-- Boolean Operations ----------------------------------------------------------

infixl 5 `or#`
infixl 6 `xor#`
infixl 7 `and#`

-- | Boolean "and" for 'Bool#'.
--
-- @since 1.0.0
and# :: Bool# -> Bool# -> Bool#
and# (B# x) (B# y) = B# (Prim.andI# x y)

-- | Boolean "or" for 'Bool#'.
--
-- @since 1.0.0
or# :: Bool# -> Bool# -> Bool#
or# (B# x) (B# y) = B# (Prim.orI# x y)

-- | Boolean "exclusive or" for 'Bool#'.
--
-- @since 1.0.0
xor# :: Bool# -> Bool# -> Bool#
xor# (B# x) (B# y) = B# (x Prim./=# y)

-- | Boolean "not" for 'Bool#'.
--
-- @since 1.0.0
not# :: Bool# -> Bool#
not# (B# a) = B# (Prim.notI# a)

-- Comparison ------------------------------------------------------------------

infix 4 `eq#`, `ne#`

-- | Equality comparison of unlifted boolean values.
--
-- @since 0.1.0.0
eq# :: Bool# -> Bool# -> Bool#
eq# (B# x) (B# y) = B# (x Prim.==# y)

-- | Inequality comparison of unlifted boolean values.
--
-- @since 0.1.0.0
ne# :: Bool# -> Bool# -> Bool#
ne# (B# x) (B# y) = B# (x Prim./=# y)

infix 4 `lt#`, `le#`, `gt#`, `ge#`

-- | "Less than" comparison of unlifted boolean values.
--
-- @since 0.1.0.0
lt# :: Bool# -> Bool# -> Bool#
lt# (B# x) (B# y) = B# (x Prim.<# y)

-- | "Less than or equal to" to comparison of unlifted boolean values.
--
-- @since 0.1.0.0
le# :: Bool# -> Bool# -> Bool#
le# (B# x) (B# y) = B# (x Prim.<=# y)

-- | "Greater than" comparison of unlifted boolean values.
--
-- @since 0.1.0.0
gt# :: Bool# -> Bool# -> Bool#
gt# (B# x) (B# y) = B# (x Prim.># y)

-- | "Greater than or equal to" to comparison of unlifted boolean values.
--
-- @since 0.1.0.0
ge# :: Bool# -> Bool# -> Bool#
ge# (B# x) (B# y) = B# (x Prim.>=# y)

-- Conversion ------------------------------------------------------------------

-- | TODO
--
-- @since 1.0.0
fromBool# :: Bool -> Bool#
fromBool# True = True#
fromBool# False = False#

-- | TODO
--
-- @since 1.0.0
toBool# :: Bool# -> Bool
toBool# True# = True
toBool# _ = False

-- Conversion - Integer --------------------------------------------------------

-- | Converts an @Int#@ to an unlifted boolean.
--
-- @since 1.0.0
fromInt# :: Int# -> Bool#
fromInt# x = B# (1# Prim.==# x)

-- | Converts an unlifted boolean to an @Int#@.
--
-- @since 1.0.0
toInt# :: Bool# -> Int#
toInt# (B# x) = x

-- | Converts an @Int8#@ to an unlifted boolean.
--
-- @since 1.0.0
fromInt8# :: Int8# -> Bool#
fromInt8# x = fromInt# (Prim.int8ToInt# x)

-- | Converts an unlifted boolean to an @Int8#@.
--
-- @since 1.0.0
toInt8# :: Bool# -> Int8#
toInt8# x = Prim.intToInt8# (toInt# x)

-- | Converts an @Int16#@ to an unlifted boolean.
--
-- @since 1.0.0
fromInt16# :: Int16# -> Bool#
fromInt16# x = fromInt# (Prim.int16ToInt# x)

-- | Converts an unlifted boolean to an @Int16#@.
--
-- @since 1.0.0
toInt16# :: Bool# -> Int16#
toInt16# x = Prim.intToInt16# (toInt# x)

-- | Converts an @Int32#@ to an unlifted boolean.
--
-- @since 1.0.0
fromInt32# :: Int32# -> Bool#
fromInt32# x = fromInt# (Prim.int32ToInt# x)

-- | Converts an unlifted boolean to an @Int32#@.
--
-- @since 1.0.0
toInt32# :: Bool# -> Int32#
toInt32# x = Prim.intToInt32# (toInt# x)

-- Conversion - Word -----------------------------------------------------------

-- | Converts an @Word#@ to an unlifted boolean.
--
-- @since 1.0.0
fromWord# :: Word# -> Bool#
fromWord# x = B# (Prim.eqWord# 1## x)

-- | Converts an unlifted boolean to an @Word#@.
--
-- @since 1.0.0
toWord# :: Bool# -> Word#
toWord# (B# x) = Prim.int2Word# x

-- | Converts an @Word8#@ to an unlifted boolean.
--
-- @since 1.0.0
fromWord8# :: Word8# -> Bool#
fromWord8# x = fromWord# (Prim.word8ToWord# x) 

-- | Converts an unlifted boolean to an @Word8#@.
--
-- @since 1.0.0
toWord8# :: Bool# -> Word8#
toWord8# x = Prim.wordToWord8# (toWord# x)

-- | Converts an @Word16#@ to an unlifted boolean.
--
-- @since 1.0.0
fromWord16# :: Word16# -> Bool#
fromWord16# x = fromWord# (Prim.word16ToWord# x) 

-- | Converts an unlifted boolean to an @Word16#@.
--
-- @since 1.0.0
toWord16# :: Bool# -> Word16#
toWord16# x = Prim.wordToWord16# (toWord# x)

-- | Converts an @Word32#@ to an unlifted boolean.
--
-- @since 1.0.0
fromWord32# :: Word32# -> Bool#
fromWord32# x = fromWord# (Prim.word32ToWord# x) 

-- | Converts an unlifted boolean to an @Word32#@.
--
-- @since 1.0.0
toWord32# :: Bool# -> Word32#
toWord32# x = Prim.wordToWord32# (toWord# x)