{
  nix-filter,
  lib,
  buildDunePackage,
  tezos-micheline,
  alcotest,
  binaryen,
  proto-alpha-utils,
  emscripten,
  ppx_blob,
  llvm,
  pkgs
}:
buildDunePackage rec {
  pname = "deku";
  version = "1.0.0";

  src = with nix-filter.lib;
    filter {
      root = ../..;
      include = [
        "deku.opam"
        "deku-c/tunac"
        "dune-project"
      ];
    };

  propagatedBuildInputs = [
    tezos-micheline
    binaryen
    proto-alpha-utils
    # llvm
    pkgs.gdb
    pkgs.cargo
    pkgs.rustup
    pkgs.wabt
  ];

  buildInputs = [
    binaryen
  ];

  checkInputs = [
    alcotest
  ];

  doCheck = false;

  meta.mainProgram = "tunac";
}
