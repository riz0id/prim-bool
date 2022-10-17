{ ghc ? "ghc924" }:

let 
  pkgs = import ./default.nix { 
    inherit ghc; 
  };
in pkgs.prim-bool.env.overrideAttrs (self: {
  buildInputs = self.buildInputs ++ (with pkgs; [ 
    pkgs.cabal-install
    pkgs.clang
    pkgs.fourmolu
    pkgs.haskell-language-server
    pkgs.llvm
  ]);
})
