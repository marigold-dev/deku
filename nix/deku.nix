{ pkgs, stdenv, lib, doCheck ? true, npmPackages, nodejs ? pkgs.nodejs }:

let ocamlPackages = pkgs.ocaml-ng.ocamlPackages_5_00;

in ocamlPackages.buildDunePackage rec {
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

  nativeBuildInputs = [ nodejs npmPackages ]
    ++ (with ocamlPackages; [ utop reason ]);

  propagatedBuildInputs = with ocamlPackages;
    [
      cmdliner
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
      ppx_blob
      secp256k1-internal
      bigstring
      domainslib
      prometheus
      prometheus-dream
    ]
    # checkInputs are here because when cross compiling dune needs test dependencies
    # but they are not available for the build phase. The issue can be seen by adding strictDeps = true;.
    ++ checkInputs ++ [ npmPackages ];

  checkInputs = with ocamlPackages; [ alcotest qcheck qcheck-alcotest rely ];
}
