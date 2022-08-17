{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}

module Main (main) where

import Hedgehog (Property, forAll, property, (===))
import qualified Hedgehog.Gen as Gen
import Test.Tasty (TestName, TestTree, defaultMain, testGroup)
import qualified Test.Tasty.Hedgehog as Tasty

import Data.Bits (xor)
import Data.Bool.Prim

#if MIN_VERSION_tasty_hedgehog(1,2,0)

import Data.String (fromString)

#endif

--------------------------------------------------------------------------------

main :: IO ()
main = defaultMain testTree

testTree :: TestTree
testTree =
  testGroup
    "Test"
    [ testGroup
        "Conversion"
        [ prop "Bool" $ property do
            x <- forAll Gen.bool
            x === toBool# (fromBool# x)
        , prop "String" $ property do
            x <- forAll (Gen.element ["False#", "True#"])
            x === show# (unsafeRead# x)
        ]
    , testGroup
        "Logical"
        [ prop "and#" $ operator and# (&&)
        , prop "or#" $ operator or# (||)
        , prop "xor#" $ operator xor# xor
        , prop "not#" $ unary not# not
        ]
    , testGroup
        "Comparison"
        [ prop "gt#" $ operator gt# (>)
        , prop "ge#" $ operator ge# (>=)
        , prop "eq#" $ operator eq# (==)
        , prop "ne#" $ operator ne# (/=)
        , prop "lt#" $ operator lt# (<)
        , prop "le#" $ operator le# (<=)
        ]
    ]

#if MIN_VERSION_tasty_hedgehog(1,2,0)

prop :: TestName -> Property -> TestTree
prop name = Tasty.testPropertyNamed name (fromString name)

#else

prop :: TestName -> Property -> TestTree
prop = Tasty.testProperty

#endif

unary :: (Bool# -> Bool#) -> (Bool -> Bool) -> Property
unary op# op = property $ do
  x@(fromBool# -> x#) <- forAll Gen.bool
  op x === toBool# (op# x#)

operator :: (Bool# -> Bool# -> Bool#) -> (Bool -> Bool -> Bool) -> Property
operator op# op = property $ do
  x@(fromBool# -> x#) <- forAll Gen.bool
  y@(fromBool# -> y#) <- forAll Gen.bool
  op x y === toBool# (op# x# y#)