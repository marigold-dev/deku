{ pkgs, doCheck ? true, nodejs, npmPackages, static ? false, nix-filter }:

let
  inherit (pkgs) lib stdenv ocamlPackages;
  libpg_query = pkgs.libpg_query.overrideAttrs (s: {
    dontDisableStatic = static;

    nativeBuildInputs = s.nativeBuildInputs ++ [ pkgs.musl ];
  });
in


with ocamlPackages; stdenv.mkDerivation rec {
  name = "deku";

  version = "0.0.0-dev";

  src = with nix-filter.lib; filter {
    root = ../..;
    include = [
      "deku.opam"
      "dune"
      "dune-project"
      "deku-p/src"
    ];
  };

  # This is the same as standard dune build but with static support
  buildPhase = ''
    echo NODE_PATH
    echo $NODE_PATH
    runHook preBuild
    echo "running ${if static then "static" else "release"} build"
    dune build --profile=${if static then "static" else "release"} ./deku-p/src/cli/deku_cli.exe
    # dune build --profile=${if static then "static" else "release"} ./deku-p/src/core/bin/node/deku_node.exe
    # dune build --profile=${if static then "static" else "release"} ./deku-p/src/core/bin/benchmark/deku_benchmark.exe
    # dune build --profile=${if static then "static" else "release"} ./deku-p/src/generate_identity/deku_generate_identity.exe
    # dune build --profile=${if static then "static" else "release"} ./deku-p/src/helper/deku_helper.exe
    # dune build --profile=${if static then "static" else "release"} ./deku-p/src/vm_protocol_tester/deku_vm_protocol_tester.exe
    runHook postBuild
  '';

  installPhase = ''
    runHook preInstall
    mkdir -p $out/bin
    cp _build/default/deku-p/src/cli/deku_cli.exe $out/bin/deku-cli
    # cp _build/default/deku-p/src/core/bin/node/deku_node.exe $out/bin/deku-node
    # cp _build/default/deku-p/src/core/bin/benchmark/deku_benchmark.exe $out/bin/deku-benchmark
    # cp _build/default/deku-p/src/generate_identity/deku_generate_identity.exe $out/bin/deku-generate-identity
    # cp _build/default/deku-p/src/helper/deku_helper.exe $out/bin/deku-helper
    # cp _build/default/deku-p/src/vm_protocol_tester/deku_vm_protocol_tester.exe $out/bin/deku-vm-protocol-tester
    runHook postInstall
  '';

  checkPhase = ''
    runHook preInstall
    dune build --profile=${if static then "static" else "release"} -p ${name} @runtest
    runHook postInstall
  '';

  nativeBuildInputs = [ nodejs dune ocaml findlib ] ++ npmPackages;

  buildInputs = [
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
    cmdliner
    ppx_blob
    data-encoding
    caqti
    caqti-driver-sqlite3
    ppx_deriving_cmdliner
    dream
    caqti-eio
    routes
    ppx_rapper
    ppx_rapper_eio
    ezgzip
  ]
  # checkInputs are here because when cross compiling dune needs test dependencies
  # but they are not available for the build phase. The issue can be seen by adding strictDeps = true;.
  ++ checkInputs;

  checkInputs = [ alcotest ];

  meta.mainProgram = "deku-node";
}
