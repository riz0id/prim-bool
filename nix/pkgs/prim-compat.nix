{ mkDerivation, base, fetchgit, ghc-prim, hedgehog, lib, tasty
, tasty-hedgehog
}:
mkDerivation {
  pname = "prim-compat";
  version = "1.0.0";
  src = fetchgit {
    url = "https://github.com/riz0id/prim-compat";
    sha256 = "17c8dza85im4lhgndijbmfpbjr97kanl1gffvh1n8a51hl687qmh";
    rev = "b72cd5531fa0d932641ac2c5cc106c23f204623b";
    fetchSubmodules = true;
  };
  libraryHaskellDepends = [ base ghc-prim ];
  testHaskellDepends = [ base hedgehog tasty tasty-hedgehog ];
  homepage = "https://github.com/riz0id/prim-compat";
  description = "Lightweight ghc-prim wrappers for backwards compatibility";
  license = lib.licenses.isc;
}
