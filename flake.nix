{
  description = "prim-bool";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixpkgs-unstable";

    flake-utils.url = "github:numtide/flake-utils";

    gitignore = {
      url = "github:hercules-ci/gitignore.nix";
      inputs.nixpkgs.follows = "nixpkgs";
    };
  };

  outputs = { self, nixpkgs, flake-utils, gitignore }:
    {
      overlays = {
        default = import ./nix/overlays/haskell.nix {
          inherit gitignore;
        };
      };
    }
    // flake-utils.lib.eachSystem ["x86_64-linux" "x86_64-darwin"] (system:
      let
        mapcat = f: lst: builtins.foldl' (l: r: l // r) {} (map f lst);

        pkgs = import nixpkgs {
          overlays = [
            self.overlays.default
          ];

          inherit system;
        };

        mkVersionedPackages = ghcVersion:
          pkgs.lib.attrsets.mapAttrs' (name: _: {
            name = "${name}-ghc${ghcVersion}";
            value = pkgs.haskell.packages."ghc${ghcVersion}".${name};
          }) pkgs.localHsPackages;

        versionedDevShells = mapcat pkgs.mkDevShell pkgs.ghcVersions;
      in {
        packages = mapcat mkVersionedPackages pkgs.ghcVersions;

        devShells = pkgs.hackageUploadShell // versionedDevShells // {
          default = versionedDevShells."prim-bool-dev-ghc${pkgs.defaultGHCVersion}";
        };
      });
}