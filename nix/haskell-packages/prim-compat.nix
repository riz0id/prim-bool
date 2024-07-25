{ mkDerivation, base, fetchgit, ghc-prim, hedgehog, lib, tasty
, tasty-hedgehog
}:
mkDerivation {
  pname = "prim-compat";
  version = "1.0.0";
  src = fetchgit {
    url = "https://github.com/riz0id/prim-compat";
    sha256 = "0jlisvwn8cd7z0kiw8q01d6pyqnfa11k0gf3my9sd7hn18xm592m";
    rev = "a9f7ebea272cb0cdc9a0209648a83d129d03d386";
    fetchSubmodules = true;
  };
  libraryHaskellDepends = [ base ghc-prim ];
  testHaskellDepends = [ base hedgehog tasty tasty-hedgehog ];
  homepage = "https://github.com/riz0id/prim-compat";
  description = "Lightweight ghc-prim wrappers for backwards compatibility";
  license = lib.licenses.isc;
}
