{ fetchTarball ? builtins.fetchTarball }:

# nixpkgs release 21.11 pinned on August 3rd, 2022.
# url: <https://github.com/NixOS/nixpkgs/releases/tag/21.11>
fetchTarball {
  url = "https://github.com/NixOS/nixpkgs/archive/refs/tags/21.11.tar.gz";
  sha256 = "162dywda2dvfj1248afxc45kcrg83appjd0nmdb541hl7rnncf02";
}