
module Test.Bool.Fuzz.Expr (testTree) where

import Data.Bool.Prim (toBool)

import Hedgehog (Property, forAll, property, (===))
import Hedgehog.Gen qualified as Gen

import Test.Bool.Fuzz.Expr.Core (eval, eval#)
import Test.Bool.Fuzz.Expr.Gen qualified as Gen.Expr
import Test.Compat (TestTree, testProp)

--------------------------------------------------------------------------------

testTree :: TestTree
testTree = testProp "Expr" fuzzExpr

fuzzExpr :: Property
fuzzExpr = property do
  expr <- forAll (Gen.resize 1000 Gen.Expr.expr)
  eval expr === toBool (eval# expr)