{ ghc ? "ghc921" }:

let
  nixpkgs = import nix/nixpkgs.nix { };

  extension = pkgs: {
    haskell = pkgs.haskell // {
      packages = pkgs.haskell.packages // {
        "${ghc}" = pkgs.haskell.packages."${ghc}".extend (self: _: {
          unlifted-bool = self.callCabal2nix "unlifted-bool" ./. { };
        });
      };
    };
  };

  pkgs = import nixpkgs {
    config.packageOverrides = extension;
  };
in {
  inherit (pkgs.haskell.packages."${ghc}") unlifted-bool; 
}

