{ pkgs, doCheck ? true, nodejs, npmPackages, static ? false, removeReferencesTo }:

let inherit (pkgs) lib stdenv ocamlPackages; in

with ocamlPackages; buildDunePackage rec {
  pname = "deku";
  version = "0.0.0-dev";

  src = lib.filterSource {
    src = ./..;
    dirs = [ "src" ];
    files = [ "dune-project" "deku.opam" ];
  };

  buildPhase = ''
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
    domainslib
    cmdliner
    ppx_blob
    data-encoding
    caqti
    caqti-lwt
    caqti-driver-sqlite3
    ppx_deriving_cmdliner
    landmarks
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
      $out/bin/{deku-benchmark,deku-bootstrap,deku-generate-identity,deku-node}
  '' + (if static then ''
    # If we're building statically linked binaries everything should be possible to remove
    remove-references-to \
      -t ${pkgs.gmp} \
      $out/bin/{deku-benchmark,deku-bootstrap,deku-generate-identity,deku-node}
    remove-references-to \
      -t ${pkgs.libffi} \
      $out/bin/{deku-benchmark,deku-bootstrap,deku-generate-identity,deku-node}
  '' else
    "");
}
