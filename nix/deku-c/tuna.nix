{
  nix-filter,
  lib,
  buildDunePackage,
  tezos-micheline,
  alcotest,
  binaryen,
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
