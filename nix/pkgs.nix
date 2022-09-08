{ ghc ? "ghc922" }:

let
  nixpkgs = import ./nixpkgs.nix { };

  overlay = self: super: {
    haskell = super.haskell // {
      packages = super.haskell.packages // {
        "${ghc}" = super.haskell.packages."${ghc}".override (_: {
          overrides = (new: _: {
            prim-bool   = new.callCabal2nix "prim-bool" ../. { };
            prim-compat = new.callPackage pkgs/prim-compat.nix { };
          });
        });
      };
    };
  };
in import nixpkgs {
  overlays = [ overlay ];
}