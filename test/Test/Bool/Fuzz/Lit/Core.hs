
module Test.Bool.Fuzz.Lit.Core
  ( Lit (LitBool, LitInt, LitWord),
    eval,
    eval#,
  )
where

import Data.Bool.Prim 
import Data.Kind (Type)

import GHC.Exts (Int (I#), Word (W#))

--------------------------------------------------------------------------------

data Lit :: Type where
  LitBool :: Bool -> Lit
  LitInt :: Int -> Lit
  LitWord :: Word -> Lit
  deriving (Eq)

instance Show Lit where
  show (LitBool x) = show x
  show (LitInt x@(I# x#)) =
    shows x ":" ++ shows (toBool (fromInt# x#)) ":i"
  show (LitWord x@(W# x#)) = 
    shows x ":" ++ shows (toBool (fromWord# x#)) ":w"

eval :: Lit -> Bool
eval (LitBool x) = x
eval (LitInt x) = 1 == x
eval (LitWord x) = 1 == x 

eval# :: Lit -> Bool#
eval# (LitBool x) = fromBool x
eval# (LitInt (I# x#)) = fromInt# x#
eval# (LitWord (W# x#)) = fromWord# x#