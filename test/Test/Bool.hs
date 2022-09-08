module Test.Bool (testTree) where

import Test.Compat (TestTree, testGroup)

import Test.Bool.Comparison qualified
import Test.Bool.Conversion qualified
import Test.Bool.Fuzz qualified
import Test.Bool.Logical qualified

--------------------------------------------------------------------------------

testTree :: TestTree
testTree =
  testGroup
    "Test"
    [ Test.Bool.Comparison.testTree
    , Test.Bool.Conversion.testTree
    , Test.Bool.Logical.testTree
    , Test.Bool.Fuzz.testTree
    ]
