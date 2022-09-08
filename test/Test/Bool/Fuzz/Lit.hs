{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MagicHash #-}

module Test.Bool.Fuzz.Lit
  ( Lit (LitBool, LitInt, LitWord),
    eval,
    eval#,
  )
where

import Test.Bool.Fuzz.Lit.Core (Lit (LitBool, LitInt, LitWord), eval, eval#)

--------------------------------------------------------------------------------
