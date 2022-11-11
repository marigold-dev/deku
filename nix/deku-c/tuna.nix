{
  nix-filter,
  lib,
  buildDunePackage,
  tezos-micheline,
  alcotest,
  binaryen,
  proto-alpha-utils,
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
