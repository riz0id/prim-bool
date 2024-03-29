{-# LANGUAGE CPP #-}

module Main (main) where

import Test.Tasty (defaultMain)

import Test.Bool qualified
import Test.Compat (TestTree, testGroup)

--------------------------------------------------------------------------------

main :: IO ()
main = defaultMain testTree

testTree :: TestTree
testTree =
  testGroup
    "Test"
    [ Test.Bool.testTree
    ]