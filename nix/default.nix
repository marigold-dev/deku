{ pkgs, doCheck ? true, nodejs, npmPackages }:

let inherit (pkgs) lib stdenv ocamlPackages; in
with ocamlPackages; buildDunePackage rec {
  pname = "deku";

  caqti-eio = buildDunePackage {
    pname = "caqti-eio";
    version = "n/a";
    src = builtins.fetchurl {
      url = https://github.com/anmonteiro/caqti-eio/archive/c709dad.tar.gz;
      sha256 = "0mmjms378akcs7lifpz3s82hw7g6sdxbsyqlb0yrry7as29rccsz";
    };
    propagatedBuildInputs = [ eio eio_main caqti ];
  };

  version = "0.0.0-dev";

  src = lib.filterSource {
    src = ./..;
    dirs = [ "src" ];
    files = [ "dune-project" "deku.opam" ];
  };

  nativeBuildInputs = [ nodejs ] ++ npmPackages;

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
    prometheus
    prometheus-reporter
  ]
  # checkInputs are here because when cross compiling dune needs test dependencies
  # but they are not available for the build phase. The issue can be seen by adding strictDeps = true;.
  ++ checkInputs;

  checkInputs = [ alcotest ];

  meta.mainProgram = "deku-node";
}
