{ pkgs, doCheck ? true, nodejs, npmPackages, static ? false, removeReferencesTo }:

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
    dirs = [ "deku-p" ];
    files = [ "dune-project" "deku.opam" ];
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

  nativeBuildInputs = [ nodejs removeReferencesTo ] ++ npmPackages;

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
  ]
  # checkInputs are here because when cross compiling dune needs test dependencies
  # but they are not available for the build phase. The issue can be seen by adding strictDeps = true;.
  ++ checkInputs;

  checkInputs = [ alcotest ];

  # Remove every directory which could have links to other store paths.
  # This makes the result much smaller
  isLibrary = false;
  postFixup = ''
    rm -rf $out/lib $out/share/doc
    remove-references-to \
      -t ${ocamlPackages.ocaml} \
      $out/bin/{deku-benchmark,deku-generate-identity,deku-helper,deku-node}
  '' + (if static then ''
    # If we're building statically linked binaries everything should be possible to remove
    remove-references-to \
      -t ${pkgs.gmp} \
      $out/bin/{deku-benchmark,deku-generate-identity,deku-helper,deku-node}
    remove-references-to \
      -t ${pkgs.libffi} \
      $out/bin/{deku-benchmark,deku-generate-identity,deku-helper,deku-node}
  '' else
    "");

  meta.mainProgram = "deku-node";
}
