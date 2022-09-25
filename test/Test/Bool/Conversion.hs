
module Test.Bool.Conversion
  ( testTree,
  )
where

import Data.Bool.Prim 

import GHC.Exts (Int (I#), Word (W#))

import Hedgehog (forAll, property, (===))
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range

import Test.Compat (TestTree, testGroup, testProp)

--------------------------------------------------------------------------------

testTree :: TestTree
testTree =
  testGroup
    "Conversion"
    [ testProp "Int" $ property do
        x@(I# x#) <- forAll (Gen.int Range.constantBounded)
        if x == 1
          then I# (toInt# (fromInt# x#)) === 1
          else I# (toInt# (fromInt# x#)) === 0
    , testProp "Word" $ property do
        x@(W# x#) <- forAll (Gen.word Range.constantBounded)
        if x == 1
          then W# (toWord# (fromWord# x#)) === 1
          else W# (toWord# (fromWord# x#)) === 0
    , testProp "String" $ property do
        x <- forAll (Gen.element ["False#", "True#"])
        x === show# (unsafeRead# x)
    ]
