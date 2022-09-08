{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE MagicHash #-}

module Main (main, inbounds32, inbounds32#) where

import Data.Bool.Prim (Bool#)
import Data.Bool.Prim qualified as Bool

import GHC.Exts (Int (I#), Int#)
import GHC.Exts qualified as GHC

lt# :: Int# -> Int# -> Bool#
lt# x# y# = Bool.unsafeFromInt# (x# GHC.<# y#)

le# :: Int# -> Int# -> Bool#
le# x# y# = Bool.unsafeFromInt# (x# GHC.<=# y#)

inbounds32# :: Int# -> Bool#
inbounds32# x# = Bool.and# (le# 0# x#) (lt# x# 32#)

inbounds32 :: Int -> Bool
inbounds32 (I# x#) = Bool.toBool (inbounds32# x#)

main :: IO ()
main = do 
  print (inbounds32 0)
  print (inbounds32 15)
  print (inbounds32 $ negate 1)
  print (inbounds32 32)