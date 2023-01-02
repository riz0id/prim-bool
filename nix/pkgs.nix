args:

import (import ./nixpkgs.nix) {
  config.packageOverrides = pkgs: 
    pkgs.lib.composeManyExtensions (map (f: f args) [  
      (import exts/prim-compat.nix)
      (import exts/prim-bool.nix)
    ]) pkgs pkgs;
}