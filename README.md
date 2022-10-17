<div align="center">

# prim-bool: unboxed booleans

[![GHC 9.2.4](https://github.com/riz0id/prim-compat/actions/workflows/ghc924.yml/badge.svg)](https://github.com/riz0id/prim-compat/actions/workflows/ghc924.yml) 

</div>

`prim-bool` packages an unboxed representation of booleans `Bool#`. `Bool#` is an `Int#` wrapper intended to replace pattern matching on `0#` or `1#` literals (as is done in `ghc-prim`) with the safer pattern synonyms `False#` and `True#` in primitive boolean functions. 

## Acknowledgements

Credit to @Icelandjack [for the definition of `Bool#`](https://gitlab.haskell.org/ghc/ghc/-/wikis/unlifted-data-types).
