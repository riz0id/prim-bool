
# `unlifted-bool`

`unlifted-bool` repackages `ghc-prim` comparisons on unlifted primitives to use a `Bool#` primitive with `True#` and `False#` patterns that can be scrutinized safely.

``` haskell
-- less than or equal 'Int#' comparison via `unlifted-bool`
leInt# :: Int# -> Int# -> Bool#

f# :: Int# -> Int# -> Int#
f# a b = 
  case leInt# a b of 
    True#  -> 100#
    False# -> 0#

-- less than or equal 'Int#' comparison via `ghc-prim` 
(<=#) :: Int# -> Int# -> Int#

f# :: Int# -> Int# -> Int#
f# a b =
  case a <=# b of 
    1# -> 100#
    _  -> 0#
```

## Acknowledgements

Credit to @Icelandjack [for the definition of `Bool#`](https://gitlab.haskell.org/ghc/ghc/-/wikis/unlifted-data-types):

```haskell
newtype Bool# = B# Int#
{-# COMPLETE True#, False# #-}

pattern True#  = B# 1#
pattern False# = B# 0#
```
