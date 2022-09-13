{ pkgs, doCheck ? true, nodejs, npmPackages }:

let inherit (pkgs) lib stdenv ocamlPackages; in

with ocamlPackages; buildDunePackage rec {
  pname = "deku";
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
    cohttp
    cohttp-lwt-unix
    piaf
    domainslib
    cmdliner
    ppx_blob
    data-encoding
    caqti
    caqti-lwt
    caqti-driver-sqlite3
    ppx_deriving_cmdliner
  ]
  # checkInputs are here because when cross compiling dune needs test dependencies
  # but they are not available for the build phase. The issue can be seen by adding strictDeps = true;.
  ++ checkInputs;

  checkInputs = [ alcotest ];
}
