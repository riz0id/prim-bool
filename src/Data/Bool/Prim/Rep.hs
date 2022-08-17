{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_HADDOCK show-extensions #-}

-- |
-- Module      :  Data.Bool.Prim.Rep
-- Copyright   :  (c) Jacob Leach, 2022
-- License     :  ISC, see LICENSE
--
-- Maintainer  :  jacobleach@protonmail.com
-- Stability   :  stable
-- Portability :  non-portable (GHC primitives)
--
-- "Data.Bool.Prim.Rep" exports the runtime representation and sort of unboxed
-- boolean types.
--
-- The runtime representation for unboxed boolean values 'BoolRep' is a synonym
-- for 'IntRep' since GHC's prim-ops store boolean results as 'Int#' values:
--
-- @
-- -- only the integer 1 is interpreted as 'True'
-- 'GHC.Int.I#' (1# 'GHC.Prim.==#' 1#) == 1
--
-- -- any other integer is interpreted as 'False'
-- 'GHC.Int.I#' (1# 'GHC.Prim.==#' 0#) == 0
-- @
--
-- Choosing to represent unboxed boolean values as 'IntRep' means no additional
-- overhead is incurred by using unboxed booleans over 'Int#'. Choosing a
-- different representation for 'BoolRep' with only two inhabitants (such as a
-- pair of empty 'GHC.Types.TupleRep') is, in general, safer than 'IntRep'.
-- However, a sum representation's incompatibility with GHC's prim-ops
-- necessitates conversions to and from 'Int#' that can't be simplified away.
--
-- @since 1.0.0
module Data.Bool.Prim.Rep
  ( BoolType,
    BoolRep,
  )
where

import GHC.Types (RuntimeRep (IntRep), TYPE)
import GHC.Prim (Int#)

--------------------------------------------------------------------------------

-- | The sort of types that represent unboxed boolean values.
--
-- @since 1.0.0
type BoolType = TYPE BoolRep

-- | The runtime representation of unboxed boolean types.
--
-- @since 1.0.0
type BoolRep = 'IntRep