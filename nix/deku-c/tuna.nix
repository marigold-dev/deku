{
  nix-filter,
  lib,
  buildDunePackage,
  zarith,
  ppx_deriving,
  ppx_yojson_conv,
  yojson,
  wasm,
  data-encoding,
  tezos-micheline,
  core,
  core_unix,
  ppx_jane,
  alcotest,
  ppx_cstruct,
  core_bench,
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
    ppx_yojson_conv
    data-encoding
    wasm
    tezos-micheline
    ppx_cstruct
    core_bench
  ];

  buildInputs = [
    yojson
    core
    core_unix
    ppx_jane
    ppx_cstruct
    core_bench
  ];

  checkInputs = [
    alcotest
  ];

  doCheck = false;

  meta.mainProgram = "tunac";
}
