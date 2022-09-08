{-# LANGUAGE MagicHash #-}

module Test.Bool.Comparison
  ( testTree,
  )
where

import Data.Bool.Prim (eq#, ge#, gt#, le#, lt#, ne#)

import Test.Core (TestTree, homomorphic2, testGroup, testProp)

--------------------------------------------------------------------------------

testTree :: TestTree
testTree =
  testGroup
    "Comparison"
    [ testProp "gt#" $ homomorphic2 gt# (>)
    , testProp "ge#" $ homomorphic2 ge# (>=)
    , testProp "eq#" $ homomorphic2 eq# (==)
    , testProp "ne#" $ homomorphic2 ne# (/=)
    , testProp "lt#" $ homomorphic2 lt# (<)
    , testProp "le#" $ homomorphic2 le# (<=)
    ]
