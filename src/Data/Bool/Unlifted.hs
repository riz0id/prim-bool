{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE GADTSyntax #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE UnliftedNewtypes #-}

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
-- @since 0.1.0.0
module Data.Bool.Unlifted
  ( -- * Unlifted Booleans
    Bool# (False#, True#),

    -- ** Operations
    andB#,
    orB#,
    xorB#,
    notB#,

    -- ** Conversion
    fromBool,
    intToBool#,
    boolToInt#,

    -- ** Char#
    gtChar#,
    geChar#,
    eqChar#,
    neChar#,
    ltChar#,
    leChar#,

    -- ** Word8#
    gtWord8#,
    geWord8#,
    eqWord8#,
    neWord8#,
    ltWord8#,
    leWord8#,

    -- ** Word16#
    gtWord16#,
    geWord16#,
    eqWord16#,
    neWord16#,
    ltWord16#,
    leWord16#,

    -- ** Word32#
    gtWord32#,
    geWord32#,
    eqWord32#,
    neWord32#,
    ltWord32#,
    leWord32#,

    -- ** Word#
    gtWord#,
    geWord#,
    eqWord#,
    neWord#,
    ltWord#,
    leWord#,

    -- ** Double#
    gtDouble#,
    geDouble#,
    eqDouble#,
    neDouble#,
    ltDouble#,
    leDouble#,

    -- ** Float#
    gtFloat#,
    geFloat#,
    eqFloat#,
    neFloat#,
    ltFloat#,
    leFloat#,
  )
where

import GHC.Prim
  ( Char#,
    Double#,
    Float#,
    Int#,
    Word#,
    Word16#,
    Word32#,
    Word8#,
  )
import qualified GHC.Prim as Prim

import GHC.Types
  ( RuntimeRep
      ( DoubleRep,
        FloatRep,
        IntRep,
        Word16Rep,
        Word32Rep,
        Word8Rep,
        WordRep
      ),
    TYPE,
  )

-- -----------------------------------------------------------------------------

-- | Unlifted Boolean type
--
-- @since 0.1.0.0
newtype Bool# :: TYPE 'IntRep where
  B# :: Int# -> Bool#

{-# COMPLETE True#, False# #-}

-- | Unlifted 'True' pattern synonym.
--
-- @since 0.1.0.0
pattern True# :: Bool#
pattern True# = B# 1#

-- | Unlifted 'False' pattern synonym.
--
-- @since 0.1.0.0
pattern False# :: Bool#
pattern False# = B# 0#

-- -----------------------------------------------------------------------------
--
-- Bool# Operations
--

infixr 2 `orB#`
infixr 3 `andB#`

-- | 'Bool#' and.
--
-- @since 0.1.0.0
andB# :: Bool# -> Bool# -> Bool#
andB# (B# a) (B# b) = intToBool# (Prim.andI# a b)

-- | 'Bool#' or.
--
-- @since 0.1.0.0
orB# :: Bool# -> Bool# -> Bool#
orB# (B# a) (B# b) = intToBool# (Prim.orI# a b)

-- | 'Bool#' exclusive or.
--
-- @since 0.1.0.0
xorB# :: Bool# -> Bool# -> Bool#
xorB# (B# a) (B# b) = intToBool# (Prim.xorI# a b)

-- | 'Bool#' complement.
--
-- @since 0.1.0.0
notB# :: Bool# -> Bool#
notB# (B# a) = intToBool# (Prim.notI# a)

-- -----------------------------------------------------------------------------
--
-- Conversion
--

-- |
--
-- @since 0.1.0.0
fromBool :: Bool -> Bool#
fromBool = \case True -> B# 1#; False -> B# 0#

-- |
--
-- @since 0.1.0.0
intToBool# :: Int# -> Bool#
intToBool# = \case 1# -> True#; _ -> False#

-- |
--
-- @since 0.1.0.0
boolToInt# :: Bool# -> Int#
boolToInt# = \case True# -> 1#; _ -> 0#

-- -----------------------------------------------------------------------------
--
-- 'Char#' Relations
--

infix 4 `gtChar#`, `geChar#`, `eqChar#`, `ltChar#`, `leChar#`

-- | @since 0.1.0.0
gtChar# :: Char# -> Char# -> Bool#
gtChar# a b = relW# Prim.gtChar# a b

-- | @since 0.1.0.0
geChar# :: Char# -> Char# -> Bool#
geChar# a b = relW# Prim.geChar# a b

-- | @since 0.1.0.0
eqChar# :: Char# -> Char# -> Bool#
eqChar# a b = relW# Prim.eqChar# a b

-- | @since 0.1.0.0
neChar# :: Char# -> Char# -> Bool#
neChar# a b = relW# Prim.neChar# a b

-- | @since 0.1.0.0
ltChar# :: Char# -> Char# -> Bool#
ltChar# a b = relW# Prim.ltChar# a b

-- | @since 0.1.0.0
leChar# :: Char# -> Char# -> Bool#
leChar# a b = relW# Prim.leChar# a b

-- -----------------------------------------------------------------------------
--
-- 'Word8#' Relations
--

infix 4 `gtWord8#`, `geWord8#`, `eqWord8#`, `ltWord8#`, `leWord8#`

-- | @since 0.1.0.0
gtWord8# :: Word8# -> Word8# -> Bool#
gtWord8# a b = relW8# Prim.gtWord8# a b

-- | @since 0.1.0.0
geWord8# :: Word8# -> Word8# -> Bool#
geWord8# a b = relW8# Prim.geWord8# a b

-- | @since 0.1.0.0
eqWord8# :: Word8# -> Word8# -> Bool#
eqWord8# a b = relW8# Prim.eqWord8# a b

-- | @since 0.1.0.0
neWord8# :: Word8# -> Word8# -> Bool#
neWord8# a b = relW8# Prim.neWord8# a b

-- | @since 0.1.0.0
ltWord8# :: Word8# -> Word8# -> Bool#
ltWord8# a b = relW8# Prim.ltWord8# a b

-- | @since 0.1.0.0
leWord8# :: Word8# -> Word8# -> Bool#
leWord8# a b = relW8# Prim.leWord8# a b

-- -----------------------------------------------------------------------------
--
-- 'Word16#' Relations
--

infix 4 `gtWord16#`, `geWord16#`, `eqWord16#`, `ltWord16#`, `leWord16#`

-- | @since 0.1.0.0
gtWord16# :: Word16# -> Word16# -> Bool#
gtWord16# a b = relW16# Prim.gtWord16# a b

-- | @since 0.1.0.0
geWord16# :: Word16# -> Word16# -> Bool#
geWord16# a b = relW16# Prim.geWord16# a b

-- | @since 0.1.0.0
eqWord16# :: Word16# -> Word16# -> Bool#
eqWord16# a b = relW16# Prim.eqWord16# a b

-- | @since 0.1.0.0
neWord16# :: Word16# -> Word16# -> Bool#
neWord16# a b = relW16# Prim.neWord16# a b

-- | @since 0.1.0.0
ltWord16# :: Word16# -> Word16# -> Bool#
ltWord16# a b = relW16# Prim.ltWord16# a b

-- | @since 0.1.0.0
leWord16# :: Word16# -> Word16# -> Bool#
leWord16# a b = relW16# Prim.leWord16# a b

-- -----------------------------------------------------------------------------
--
-- 'Word32#' Relations
--

-- | @since 0.1.0.0
gtWord32# :: Word32# -> Word32# -> Bool#
gtWord32# a b = relW32# Prim.gtWord32# a b

-- | @since 0.1.0.0
geWord32# :: Word32# -> Word32# -> Bool#
geWord32# a b = relW32# Prim.geWord32# a b

-- | @since 0.1.0.0
eqWord32# :: Word32# -> Word32# -> Bool#
eqWord32# a b = relW32# Prim.eqWord32# a b

-- | @since 0.1.0.0
neWord32# :: Word32# -> Word32# -> Bool#
neWord32# a b = relW32# Prim.neWord32# a b

-- | @since 0.1.0.0
ltWord32# :: Word32# -> Word32# -> Bool#
ltWord32# a b = relW32# Prim.ltWord32# a b

-- | @since 0.1.0.0
leWord32# :: Word32# -> Word32# -> Bool#
leWord32# a b = relW32# Prim.leWord32# a b

-- -----------------------------------------------------------------------------
--
-- 'Word#' Relations
--

infix 4 `gtWord#`, `geWord#`, `eqWord#`, `ltWord#`, `leWord#`

-- | @since 0.1.0.0
gtWord# :: Word# -> Word# -> Bool#
gtWord# a b = relW# Prim.gtWord# a b

-- | @since 0.1.0.0
geWord# :: Word# -> Word# -> Bool#
geWord# a b = relW# Prim.geWord# a b

-- | @since 0.1.0.0
eqWord# :: Word# -> Word# -> Bool#
eqWord# a b = relW# Prim.eqWord# a b

-- | @since 0.1.0.0
neWord# :: Word# -> Word# -> Bool#
neWord# a b = relW# Prim.neWord# a b

-- | @since 0.1.0.0
ltWord# :: Word# -> Word# -> Bool#
ltWord# a b = relW# Prim.ltWord# a b

-- | @since 0.1.0.0
leWord# :: Word# -> Word# -> Bool#
leWord# a b = relW# Prim.leWord# a b

-- -----------------------------------------------------------------------------
--
-- 'Double#' Relations
--

infix 4 `gtDouble#`, `geDouble#`, `eqDouble#`, `ltDouble#`, `leDouble#`

-- | @since 0.1.0.0
gtDouble# :: Double# -> Double# -> Bool#
gtDouble# a b = relD# (Prim.>##) a b

-- | @since 0.1.0.0
geDouble# :: Double# -> Double# -> Bool#
geDouble# a b = relD# (Prim.>=##) a b

-- | @since 0.1.0.0
eqDouble# :: Double# -> Double# -> Bool#
eqDouble# a b = relD# (Prim.==##) a b

-- | @since 0.1.0.0
neDouble# :: Double# -> Double# -> Bool#
neDouble# a b = relD# (Prim./=##) a b

-- | @since 0.1.0.0
ltDouble# :: Double# -> Double# -> Bool#
ltDouble# a b = relD# (Prim.<##) a b

-- | @since 0.1.0.0
leDouble# :: Double# -> Double# -> Bool#
leDouble# a b = relD# (Prim.<=##) a b

-- -----------------------------------------------------------------------------
--
-- 'Float#' Relations
--

infix 4 `gtFloat#`, `geFloat#`, `eqFloat#`, `ltFloat#`, `leFloat#`

-- | @since 0.1.0.0
gtFloat# :: Float# -> Float# -> Bool#
gtFloat# a b = relF# Prim.gtFloat# a b

-- | @since 0.1.0.0
geFloat# :: Float# -> Float# -> Bool#
geFloat# a b = relF# Prim.geFloat# a b

-- | @since 0.1.0.0
eqFloat# :: Float# -> Float# -> Bool#
eqFloat# a b = relF# Prim.eqFloat# a b

-- | @since 0.1.0.0
neFloat# :: Float# -> Float# -> Bool#
neFloat# a b = relF# Prim.neFloat# a b

-- | @since 0.1.0.0
ltFloat# :: Float# -> Float# -> Bool#
ltFloat# a b = relF# Prim.ltFloat# a b

-- | @since 0.1.0.0
leFloat# :: Float# -> Float# -> Bool#
leFloat# a b = relF# Prim.leFloat# a b

-- -----------------------------------------------------------------------------
--
-- Internal
--

relW# :: forall (a :: TYPE 'WordRep). (a -> a -> Int#) -> (a -> a -> Bool#)
relW# r x y = intToBool# (x `r` y)

relW8# :: forall (a :: TYPE 'Word8Rep). (a -> a -> Int#) -> (a -> a -> Bool#)
relW8# r x y = intToBool# (x `r` y)

relW16# :: forall (a :: TYPE 'Word16Rep). (a -> a -> Int#) -> (a -> a -> Bool#)
relW16# r x y = intToBool# (x `r` y)

relW32# :: forall (a :: TYPE 'Word32Rep). (a -> a -> Int#) -> (a -> a -> Bool#)
relW32# r x y = intToBool# (x `r` y)

relD# :: forall (a :: TYPE 'DoubleRep). (a -> a -> Int#) -> (a -> a -> Bool#)
relD# r x y = intToBool# (x `r` y)

relF# :: forall (a :: TYPE 'FloatRep). (a -> a -> Int#) -> (a -> a -> Bool#)
relF# r x y = intToBool# (x `r` y)
