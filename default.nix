{ ghc ? "ghc922" }:

let
  nixpkgs = import nix/nixpkgs.nix { };

  extension = pkgs: {
    haskell = pkgs.haskell // {
      packages = pkgs.haskell.packages // {
        "${ghc}" = pkgs.haskell.packages."${ghc}".extend (self: _: {
          prim-bool = self.callCabal2nix "prim-bool" ./. { };
        });
      };
    };
  };

  pkgs = import nixpkgs {
    config.packageOverrides = extension;
  };
in {
  inherit (pkgs.haskell.packages."${ghc}") prim-bool; 
  inherit (pkgs) cabal-install clang llvm;
}

