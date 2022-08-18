{ ghc ? "ghc922" }:

let
  pkgs = import nix/pkgs.nix { 
    inherit ghc;
  };
in {
  inherit (pkgs.haskell.packages."${ghc}") 
    prim-bool; 
    
  inherit (pkgs) 
    cabal-install 
    clang 
    llvm;
}

