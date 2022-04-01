{ pkgs, stdenv, lib, doCheck ? true, npmPackages, nodejs ? pkgs.nodejs }:

let
  dream' = pkgs.ocamlPackages.dream.overrideAttrs (o: {
    propagatedBuildInputs = o.propagatedBuildInputs
      ++ [ pkgs.ocamlPackages.ppx_inline_test ];
  });
in

pkgs.ocamlPackages.buildDunePackage {
  pname = "sidechain";
  version = "0.0.0-dev";

  src = lib.filterSource {
    src = ./..;
    dirs = [ "src" "ppx_let_binding" "ppx_lambda_vm" "tests" ];
    files =
      [ "dune-project" "sidechain.opam" "package.json" "package-lock.json" ];
  };

  configurePhase = ''
    export PATH=${npmPackages}/node_modules/.bin:$PATH
    export NODE_PATH=${npmPackages}/node_modules

    ln -s ${npmPackages}/node_modules ./node_modules
  '';

  inherit doCheck;

  nativeBuildInputs = [ nodejs ] ++ (with pkgs.ocamlPackages; [
    cmdliner
    utop
    reason
  ]);

  buildInputs = with pkgs.ocamlPackages; [
    ppx_deriving
    ppx_deriving_yojson
    lwt
    dream
    mirage-crypto
    mirage-crypto-pk
    mirage-crypto-rng
    mirage-crypto-ec
    piaf-dream-compat
    dream'
    mrmime
    hex
    tezos-micheline
    digestif
    ppx_blob
    secp256k1-internal
    bigstring
    domainslib
  ];

  propagatedBuildInputs = [ npmPackages pkgs.ocamlPackages.ppx_inline_test ];

  checkInputs = with pkgs.ocamlPackages; [
    alcotest
    qcheck
    qcheck-alcotest
    rely
  ];
}
