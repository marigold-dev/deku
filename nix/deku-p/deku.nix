{ pkgs, doCheck ? true, nodejs, npmPackages, nix-filter }:

let inherit (pkgs) lib stdenv ocamlPackages; in
with ocamlPackages; buildDunePackage rec {
  pname = "deku";

  version = "0.0.0-dev";

  src = with nix-filter.lib; filter {
    root = ../..;
    include = [
      "deku.opam"
      "deku-p/src"
      "dune-project"
    ];
  };

  nativeBuildInputs = [ nodejs ] ;# ++ npmPackages;

  propagatedBuildInputs = [
    tezos-micheline
    ppx_deriving
    ppx_yojson_conv
    zarith
    digestif
    mirage-crypto
    mirage-crypto-ec
    mirage-crypto-rng
    secp256k1-internal
    piaf
    domainslib
    cmdliner
    ppx_blob
    data-encoding
    caqti
    caqti-driver-sqlite3
    ppx_deriving_cmdliner
    dream
    caqti-eio
    routes
    js_of_ocaml
    ppx_jane
    wasm
  ]
  # checkInputs are here because when cross compiling dune needs test dependencies
  # but they are not available for the build phase. The issue can be seen by adding strictDeps = true;.
  ++ checkInputs;

  checkInputs = [ alcotest ];

  meta.mainProgram = "deku-node";
}
