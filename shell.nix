{ ghc ? "ghc921" }:

let 
  pkgs = import ./default.nix { 
    inherit ghc; 
  };
in pkgs.unlifted-bool.env
