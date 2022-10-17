{ ghc }:


import (import ./nixpkgs.nix) {
  config.packageOverrides = pkgs: 
    pkgs.lib.composeManyExtensions [  
      (import exts/prim-compat.nix {
        inherit ghc;
      })
      (import exts/prim-bool.nix {
        inherit ghc;
      })
      (import exts/text.nix {
        inherit ghc;
      })
    ] pkgs pkgs;
}