
# `unlifted-bool`

`unlifted-bool` packages an unlifted representation of booleans `Bool#`. `Bool#` is an `Int#` wrapper intended to replace pattern matching on `0#` or `1#` literals (as is done in `ghc-prim`) with the safer pattern synonyms `False#` and `True#` in primitive boolean functions. 

## Acknowledgements

Credit to @Icelandjack [for the definition of `Bool#`](https://gitlab.haskell.org/ghc/ghc/-/wikis/unlifted-data-types).
