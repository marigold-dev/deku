{
  nix-filter,
  lib,
  buildDunePackage,
  zarith,
  ppx_deriving,
  wasm,
  data-encoding,
  tezos-micheline,
  core,
  core_unix,
  ppx_jane,
  alcotest,
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
    zarith
    ppx_deriving
    data-encoding
    wasm
    tezos-micheline
  ];

  buildInputs = [
    core
    core_unix
    ppx_jane
  ];

  checkInputs = [
    alcotest
  ];

  doCheck = false;

  meta.mainProgram = "tunac";
}
