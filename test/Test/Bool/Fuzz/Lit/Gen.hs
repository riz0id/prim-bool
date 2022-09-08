{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MagicHash #-}

module Test.Bool.Fuzz.Lit.Gen
  ( lit,
  )
where

import Hedgehog (MonadGen, Gen)
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range

import Test.Bool.Fuzz.Lit.Core (Lit (LitBool, LitInt, LitWord))

--------------------------------------------------------------------------------

lit :: MonadGen m => m Lit
lit = Gen.choice [bool, int, word]
{-# SPECIALIZE INLINE lit :: Gen Lit #-}

bool :: MonadGen m => m Lit
bool = LitBool <$> Gen.bool
{-# SPECIALIZE INLINE bool :: Gen Lit #-}

int :: MonadGen m => m Lit
int = LitInt <$> Gen.int Range.constantBounded
{-# SPECIALIZE INLINE int :: Gen Lit #-}

word :: MonadGen m => m Lit
word = LitWord <$> Gen.word Range.constantBounded
{-# SPECIALIZE INLINE word :: Gen Lit #-}