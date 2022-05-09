{ pkgs, stdenv, lib, removeReferencesTo, doCheck ? true, cacert, npmPackages
, nodejs ? pkgs.nodejs, static ? false }:

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

  # This is the same as standard dune build but with static support
  buildPhase = ''
    runHook preBuild
    echo "running ${if static then "static" else "release"} build"
    dune build -p ${pname} --profile=${if static then "static" else "release"}
    runHook postBuild
  '';

  inherit doCheck;

  nativeBuildInputs = [ nodejs npmPackages removeReferencesTo ]
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
      piaf
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
      cacert
      core_bench
      memtrace
      landmarks
      benchmark
    ]
    # checkInputs are here because when cross compiling dune needs test dependencies
    # but they are not available for the build phase. The issue can be seen by adding strictDeps = true;.
    ++ checkInputs ++ [ npmPackages ];

  checkInputs = with ocamlPackages; [ alcotest qcheck qcheck-alcotest rely ];

  # Remove every directory which could have links to other store paths.
  # This makes the result much smaller
  isLibrary = false;
  postFixup = ''
    rm -rf $out/lib $out/nix-support $out/share/doc
    remove-references-to \
      -t ${ocamlPackages.ocaml} \
      $out/bin/deku-{node,cli}
  '' + (if static then ''
    # If we're building statically linked binaries everything should be possible to remove
    remove-references-to \
      -t ${pkgs.gmp} \
      $out/bin/deku-{node,cli}
    remove-references-to \
      -t ${pkgs.libffi} \
      $out/bin/deku-{node,cli}
  '' else
    "");
}
