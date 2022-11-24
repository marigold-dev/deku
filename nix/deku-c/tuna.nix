{ nix-filter
, lib
, buildDunePackage
# This is probably a super set of what is actually needed
# TODO: maybe we should figure out how to re-use deku.nix?
, eio
, eio_main
, zarith
, ppx_deriving
, ppx_yojson_conv
, data-encoding
, wasm
, tezos-micheline
, ppx_deriving_cmdliner
, mirage-crypto
, mirage-crypto-ec
, mirage-crypto-rng
, digestif
, tezos-crypto
, secp256k1-internal
, piaf
, cmdliner
, ppx_blob
, caqti
, caqti-driver-sqlite3
, dream
, caqti-eio
, routes
, ppx_rapper
, ppx_rapper_eio
, ezgzip
, ppx_jane # TODO: do we need this?
, core
, core_unix
, yojson
, alcotest
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
        "deku-c/interpreter"
        "deku-c/wasm-vm-ocaml"
        # TODO: why isn't tuna part of the deku build? Seems like we duplicate stuff here.
        "deku-p/src/core"
        "dune-project"
      ];
      exclude = [
        "deku-p/src/core/bin"
      ];
    };

  propagatedBuildInputs = [
    eio
    eio_main
    zarith
    ppx_deriving
    ppx_yojson_conv
    data-encoding
    wasm
    tezos-micheline
    ppx_deriving_cmdliner
    mirage-crypto
    mirage-crypto-ec
    mirage-crypto-rng
    digestif
    tezos-micheline
    tezos-crypto
    ppx_deriving
    ppx_yojson_conv
    zarith
    digestif
    mirage-crypto
    mirage-crypto-ec
    mirage-crypto-rng
    secp256k1-internal
    piaf
    cmdliner
    ppx_blob
    data-encoding
    caqti
    caqti-driver-sqlite3
    ppx_deriving_cmdliner
    dream
    eio
    eio_main
    caqti-eio
    routes
    ppx_rapper
    ppx_rapper_eio
    ezgzip
    ppx_jane # TODO: do we need this?
    core
    core_unix
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

  meta.mainProgram = "tunac";
}
