{ ghc ? "ghc924" }:

let
  pkgs = import nix/pkgs.nix { 
    inherit ghc;
  };
in {
  inherit (pkgs.haskell.packages."${ghc}") 
    cabal-install 
    fourmolu
    haskell-language-server
    hlint
    prim-bool; 
    
  inherit (pkgs) 
    clang 
    llvm;
}

