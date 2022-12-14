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
}:
buildDunePackage rec {
  pname = "deku";
  version = "1.0.0";

  src = with nix-filter.lib;
    filter {
      root = ../..;
      include = [
        "deku.opam"
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
  ];

  buildInputs = [
    yojson
    core
    core_unix
    ppx_jane
  ];

  checkInputs = [
    alcotest
  ];

  doCheck = false;
}
