{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}

module Main (main) where

import Hedgehog (Property, forAll, property, (===))
import qualified Hedgehog.Gen as Gen
import Test.Tasty (TestTree, defaultMain, testGroup)
import Test.Tasty.Hedgehog (testPropertyNamed)

import Data.Bits (xor)
import Data.Bool.Prim

--------------------------------------------------------------------------------

main :: IO ()
main = defaultMain testTree

testTree :: TestTree
testTree =
  testGroup
    "Test"
    [ testGroup
        "Conversion"
        [ testPropertyNamed "Bool#" "Bool#" $ property do
            x <- forAll Gen.bool
            x === toBool# (fromBool# x)
        , testPropertyNamed "String" "String" $ property do
            x <- forAll (Gen.element ["False#", "True#"])
            x === show# (unsafeRead# x)
        ]
    , testGroup
        "Logical"
        [ testPropertyNamed "and#" "and#" $ operator and# (&&)
        , testPropertyNamed "or#" "or#" $ operator or# (||)
        , testPropertyNamed "xor#" "xor#" $ operator xor# xor
        , testPropertyNamed "not#" "not#" $ unary not# not
        ]
    , testGroup
        "Comparison"
        [ testPropertyNamed "gt#" "gt#" $ operator gt# (>)
        , testPropertyNamed "ge#" "ge#" $ operator ge# (>=)
        , testPropertyNamed "eq#" "eq#" $ operator eq# (==)
        , testPropertyNamed "ne#" "ne#" $ operator ne# (/=)
        , testPropertyNamed "lt#" "lt#" $ operator lt# (<)
        , testPropertyNamed "le#" "le#" $ operator le# (<=)
        ]
    ]

unary :: (Bool# -> Bool#) -> (Bool -> Bool) -> Property
unary op# op = property $ do
  x@(fromBool# -> x#) <- forAll Gen.bool
  op x === toBool# (op# x#)

operator :: (Bool# -> Bool# -> Bool#) -> (Bool -> Bool -> Bool) -> Property
operator op# op = property $ do
  x@(fromBool# -> x#) <- forAll Gen.bool
  y@(fromBool# -> y#) <- forAll Gen.bool
  op x y === toBool# (op# x# y#)