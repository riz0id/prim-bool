{ ghc ? "ghc922" }:

let
  nixpkgs = import ./nixpkgs.nix { };

  extension = pkgs: {
    haskell = pkgs.haskell // {
      packages = pkgs.haskell.packages // {
        "${ghc}" = pkgs.haskell.packages."${ghc}".extend (self: _: {
          prim-bool = self.callCabal2nix "prim-bool" ./. { };
        });
      };
    };
  };

in import nixpkgs {
  config.packageOverrides = extension;
}