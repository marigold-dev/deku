{ pkgs, stdenv, lib, system, nix-filter, removeReferencesTo, doCheck ? true
, cacert, npmPackages, nodejs ? pkgs.nodejs, static ? false }:

let ocamlPackages = pkgs.ocaml-ng.ocamlPackages_5_00;

in ocamlPackages.buildDunePackage rec {
  pname = "sidechain";
  version = "0.0.0-dev";

  src = with nix-filter.lib;
    filter {
      root = ../.;
      include = [
        (inDirectory "src")
        (inDirectory "ppx_let_binding")
        (inDirectory "ppx_lambda_vm")
        (inDirectory "tests")
        "dune-project"
        "sidechain.opam"
        "package.json"
        "package-lock.json"
      ];
    };

  # This is the same as standard dune build but with static support
  buildPhase = ''
    echo NODE_PATH
    echo $NODE_PATH
    runHook preBuild
    echo "running ${if static then "static" else "release"} build"
    dune build -p ${pname} --profile=${if static then "static" else "release"}
    runHook postBuild
  '';

  inherit doCheck;

  nativeBuildInputs = [ nodejs removeReferencesTo ] ++ npmPackages
    ++ (with ocamlPackages; [ utop reason ]);

  buildInputs = with ocamlPackages;
    [
      cmdliner
      ppx_deriving
      ppx_deriving_yojson
      lwt
      lwt_domain
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
      benchmark
      json-logs-reporter
      feather
      wasm
    ]
    # checkInputs are here because when cross compiling dune needs test dependencies
    # but they are not available for the build phase. The issue can be seen by adding strictDeps = true;.
    ++ checkInputs;

  propagatedBuildInputs = [ cacert ];

  checkInputs = with ocamlPackages; [ alcotest qcheck qcheck-alcotest rely ];

  # Remove every directory which could have links to other store paths.
  # This makes the result much smaller
  isLibrary = false;
  postFixup = ''
    rm -rf $out/lib $out/share/doc
    remove-references-to \
      -t ${ocamlPackages.ocaml} \
      $out/bin/{asserter,check-liveness,deku-node,deku-cli}
  '' + (if static then ''
    # If we're building statically linked binaries everything should be possible to remove
    remove-references-to \
      -t ${pkgs.gmp} \
      $out/bin/{asserter,check-liveness,deku-node,deku-cli}
    remove-references-to \
      -t ${pkgs.libffi} \
      $out/bin/{asserter,check-liveness,deku-node,deku-cli}
  '' else
    "");
}
