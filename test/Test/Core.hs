{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}

module Test.Core
  ( TestTree,
    testGroup,
    testProp,
    homomorphic1,
    homomorphic2,
  )
where

import Hedgehog (Property, forAll, property, (===))
import qualified Hedgehog.Gen as Gen
import Test.Tasty (TestTree, testGroup)

import Data.Bool.Prim (Bool#, fromBool, toBool)

import Test.Compat (testProp)

--------------------------------------------------------------------------------

homomorphic1 :: (Bool# -> Bool#) -> (Bool -> Bool) -> Property
homomorphic1 op# op = property $ do
  x@(fromBool -> x#) <- forAll Gen.bool
  op x === toBool (op# x#)

homomorphic2 :: (Bool# -> Bool# -> Bool#) -> (Bool -> Bool -> Bool) -> Property
homomorphic2 op# op = property $ do
  x@(fromBool -> x#) <- forAll Gen.bool
  y@(fromBool -> y#) <- forAll Gen.bool
  op x y === toBool (op# x# y#)