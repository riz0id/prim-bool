{ ghc ? "ghc922" }:

let
  nixpkgs = import ./nixpkgs.nix { };

  overlay = self: super: {
    haskell = super.haskell // {
      packages = super.haskell.packages // {
        "${ghc}" = super.haskell.packages."${ghc}".override (old: {
          overrides = let
            sources = self.haskell.lib.packageSourceOverrides {
              prim-bool   = ../.;
              prim-compat = import pkgs/prim-compat.nix { };
            };

            default = old.overrides or (_: _: { });

          in self.lib.fold self.lib.composeExtensions default [ sources ];
        });
      };
    };
  };
in import nixpkgs {
  overlays = [ overlay ];
}