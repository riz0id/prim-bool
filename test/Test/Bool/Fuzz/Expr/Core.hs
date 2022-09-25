
module Test.Bool.Fuzz.Expr.Core
  ( Exp (ExpAnd, ExpXor, ExpOr, ExpNot, ExpLit),
    eval,
    eval#,
  )
where

import Data.Bits (complement, (.&.), xor, (.|.))
import Data.Bool.Prim ( not#, xor#, or#, and#, Bool# )
import Data.Kind ( Type )

import Test.Bool.Fuzz.Lit qualified as Lit
import Test.Bool.Fuzz.Lit.Core (Lit)

--------------------------------------------------------------------------------

data Exp :: Type where
  ExpAnd :: Exp -> Exp -> Exp
  ExpXor :: Exp -> Exp -> Exp
  ExpOr :: Exp -> Exp -> Exp
  ExpNot :: Exp -> Exp
  ExpLit :: Lit -> Exp
  deriving (Eq)

instance Show Exp where
  showsPrec i (ExpAnd x y) = 
    showParen (i < 7) (showsPrec 7 x . showString " & " . showsPrec 7 y)
  showsPrec i (ExpXor x y) = 
    showParen (i < 6) (showsPrec 6 x . showString " * " . showsPrec 6 y)
  showsPrec i (ExpOr x y) = 
    showParen (i < 5) (showsPrec 5 x . showString " | " . showsPrec 5 y)
  showsPrec i (ExpNot x) = 
    showParen (i < 0) (showString ("!" ++ show x))
  showsPrec _ (ExpLit x) = 
    showString (show x)
  {-# INLINE showsPrec #-}

eval :: Exp -> Bool
eval (ExpAnd x y) = eval x .&. eval y
eval (ExpXor x y) = xor (eval x) (eval y)
eval (ExpOr x y) = eval x .|. eval y
eval (ExpNot x) = complement (eval x)
eval (ExpLit x) = Lit.eval x

eval# :: Exp -> Bool#
eval# (ExpAnd x y) = and# (eval# x) (eval# y)
eval# (ExpXor x y) = xor# (eval# x) (eval# y)
eval# (ExpOr x y) = or# (eval# x) (eval# y)
eval# (ExpNot x) = not# (eval# x)
eval# (ExpLit x) = Lit.eval# x
