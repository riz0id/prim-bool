{ mkDerivation, base, fetchgit, ghc-prim, hedgehog, lib
, prim-compat, tasty, tasty-hedgehog, template-haskell
}:
mkDerivation {
  pname = "prim-bool";
  version = "1.0.1";
  src = fetchgit {
    url = "https://github.com/riz0id/prim-bool";
    sha256 = "0xicw17rh42yh2f8rwkb62gdv2as2c42fja5cfmafp0ifhagf7xr";
    rev = "8e2c83c925c978063bb8249a55cfc7b6b34d2d06";
    fetchSubmodules = true;
  };
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    base ghc-prim prim-compat template-haskell
  ];
  executableHaskellDepends = [ base ];
  testHaskellDepends = [ base hedgehog tasty tasty-hedgehog ];
  homepage = "https://github.com/riz0id/prim-bool";
  description = "Unboxed booleans";
  license = lib.licenses.isc;
}
