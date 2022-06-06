{ fetchTarball ? builtins.fetchTarball }:

let 
  pkg = "prim-compat";
  rev = "1.0.0";
in fetchTarball {
  url    = "https://github.com/riz0id/${pkg}/releases/download/${rev}/${pkg}-${rev}.tar.gz";
  sha256 = "0l104ypis2bjbkiwl33pw9ql7c7h4pd622g2hx5vp5fa33widgx3";
}