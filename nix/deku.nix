{ pkgs, stdenv, lib, ocamlPackages, doCheck ? true, npmPackages, nodejs ? pkgs.nodejs }:

let 
  rely = pkgs.ocaml-ng.ocamlPackages_5_00.reason-native.rely.overrideAttrs (_: {
    postPatch = ''
      substituteInPlace src/rely/TestSuiteRunner.re --replace "Pervasives" "Stdlib"
    '';
  });
in

ocamlPackages.buildDunePackage {
  pname = "sidechain";
  version = "0.0.0-dev";

  src = lib.filterSource {
    src = ./..;
    dirs = [ "src" "ppx_let_binding" "tests" ];
    files = [ "dune-project" "sidechain.opam" "package.json" "package-lock.json" ];
  };

  configurePhase = ''
    export NODE_PATH=${npmPackages}/node_modules
    export PATH=${npmPackages}/node_modules/.bin:$PATH

    ln -s ${npmPackages}/node_modules ./node_modules
  '';

  inherit doCheck;

  nativeBuildInputs = [
    nodejs
  ];

  propagatedBuildInpits = [
    npmPackages
  ];

  buildInputs = with ocamlPackages; [
    ppx_deriving
    ppx_deriving_yojson
    lwt
    dream
    mirage-crypto
    mirage-crypto-pk
    mirage-crypto-rng
    mirage-crypto-ec
    piaf-dream-compat
    mrmime
    hex
    tezos-micheline
    digestif
    cmdliner
    ppx_blob
    secp256k1-internal
    bigstring
    domainslib
    utop
    reason
  ];

  checkInputs = [
    rely
  ];
}