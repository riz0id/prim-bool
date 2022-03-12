{-# LANGUAGE MagicHash #-}

module Main (main) where

import Data.Bool.Unlifted (Bool# (True#, False#), gtInt#)
import GHC.Exts (Int (I#), Int#)

max# :: Int# -> Int# -> Int#
max# a b =
  case a `gtInt#` b of
    True# -> a
    False# -> b

main :: IO ()
main = do
  let x = I# (max# 5# 10#)
  putStrLn (show x)
