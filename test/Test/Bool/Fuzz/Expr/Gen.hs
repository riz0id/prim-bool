
module Test.Bool.Fuzz.Expr.Gen
  ( expr,
  )
where

import Control.Applicative (liftA2)

import Hedgehog (Gen, MonadGen)
import Hedgehog.Gen qualified as Gen

import Prelude hiding (and, or, not)

import Test.Bool.Fuzz.Expr.Core (Exp (ExpAnd, ExpLit, ExpNot))
import Test.Bool.Fuzz.Lit.Gen qualified as Gen.Lit

--------------------------------------------------------------------------------

expr :: MonadGen m => m Exp
expr = Gen.scale (`div` 2) do 
  Gen.choice [and, xor, or, not, lit]
{-# SPECIALIZE INLINE expr :: Gen Exp #-}

and :: MonadGen m => m Exp
and = term2 ExpAnd
{-# SPECIALIZE INLINE and :: Gen Exp #-}

xor :: MonadGen m => m Exp
xor = term2 ExpAnd
{-# SPECIALIZE INLINE xor :: Gen Exp #-}

or :: MonadGen m => m Exp
or = term2 ExpAnd
{-# SPECIALIZE INLINE or :: Gen Exp #-}

not :: MonadGen m => m Exp
not = term1 ExpNot
{-# SPECIALIZE INLINE not :: Gen Exp #-}

lit :: MonadGen m => m Exp
lit = ExpLit <$> Gen.Lit.lit 
{-# SPECIALIZE INLINE lit :: Gen Exp #-}

term1 :: MonadGen m => (Exp -> Exp) -> m Exp
term1 con = Gen.sized \s -> if s <= 0 then lit else fmap con expr 
{-# SPECIALIZE INLINE term1 :: (Exp -> Exp) -> Gen Exp #-}

term2 :: MonadGen m => (Exp -> Exp -> Exp) -> m Exp
term2 con = Gen.sized \s -> if s <= 0 then lit else liftA2 con expr expr 
{-# SPECIALIZE INLINE term2 :: (Exp -> Exp -> Exp) -> Gen Exp #-}
