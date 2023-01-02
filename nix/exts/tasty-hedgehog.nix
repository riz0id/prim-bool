{ ghc }:

final: prev: 

{
  haskell = prev.haskell // {
    packages = prev.haskell.packages // {
      "${ghc}" = prev.haskell.packages."${ghc}".extend (self: _: {
        tasty-hedgehog = self.callPackage ../pkgs/tasty-hedgehog.nix { };
      });
    };
  };
}