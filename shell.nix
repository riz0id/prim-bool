{ ghc ? "ghc922" }:

let 
  pkgs = import ./default.nix { 
    inherit ghc; 
  };
in pkgs.prim-bool.env.overrideAttrs (self: {
  buildInputs = self.buildInputs ++ [ 
    pkgs.cabal-install
    pkgs.clang
    pkgs.llvm
  ];
})
