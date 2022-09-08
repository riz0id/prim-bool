{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE ViewPatterns #-}

module Test.Bool.Logical (testTree) where

import Data.Bits (xor, (.&.), (.|.))
import Data.Bool.Prim (and#, fromBool, not#, or#, toBool, xor#)

import Hedgehog (forAll, property, (===))
import Hedgehog.Gen qualified as Gen

import Test.Core (TestTree, homomorphic1, homomorphic2, testGroup, testProp)

--------------------------------------------------------------------------------

testTree :: TestTree
testTree =
  testGroup
    "Logical"
    [ testTreeAnd
    , testTreeXor
    , testTreeOr
    , testTreeNot
    ]

testTreeAnd :: TestTree
testTreeAnd =
  testGroup
    "And"
    [ testProp "and#" $ homomorphic2 and# (&&)
    , testProp "associativity" $ property do
        x@(fromBool -> x#) <- forAll Gen.bool
        y@(fromBool -> y#) <- forAll Gen.bool
        z@(fromBool -> z#) <- forAll Gen.bool
        toBool (and# x# (and# y# z#)) === x .&. (y .&. z)
        toBool (and# (and# x# y#) z#) === (x .&. y) .&. z
    ]

testTreeXor :: TestTree
testTreeXor =
  testGroup
    "Xor"
    [ testProp "xor#" $ homomorphic2 xor# xor
    ]

testTreeOr :: TestTree
testTreeOr =
  testGroup
    "Or"
    [ testProp "or#" $ homomorphic2 or# (||)
    , testProp "associativity" $ property do
        x@(fromBool -> x#) <- forAll Gen.bool
        y@(fromBool -> y#) <- forAll Gen.bool
        z@(fromBool -> z#) <- forAll Gen.bool
        toBool (or# x# (or# y# z#)) === x .|. (y .|. z)
        toBool (or# (or# x# y#) z#) === (x .|. y) .|. z
    ]

testTreeNot :: TestTree
testTreeNot =
  testGroup
    "Or"
    [ testProp "not#" $ homomorphic1 not# not
    , testProp "involution" $ property do
        x@(fromBool -> x#) <- forAll Gen.bool
        toBool (not# (not# x#)) === x
    ]
