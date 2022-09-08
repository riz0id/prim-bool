{-# LANGUAGE MagicHash #-}

module Test.Bool.Fuzz
  ( testTree,
  )
where

import Test.Compat (TestTree, testGroup)

import Test.Bool.Fuzz.Expr qualified

--------------------------------------------------------------------------------

testTree :: TestTree
testTree =
  testGroup
    "Expr"
    [ Test.Bool.Fuzz.Expr.testTree
    ]
