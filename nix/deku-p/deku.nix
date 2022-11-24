{
  pkgs,
  doCheck ? true,
  nodejs,
  npmPackages,
  static ? false,
  removeReferencesTo,
  nix-filter,
}: let
  inherit (pkgs) lib stdenv ocamlPackages;
in
  with ocamlPackages;
    buildDunePackage rec {
      pname = "deku";

      version = "0.0.0-dev";

      src = with nix-filter.lib;
        filter {
          root = ../..;
          include = [
            "deku.opam"
            "dune"
            "deku-p/src"
            "dune-project"
            "deku-c"
          ];
          exclude =
            # TODO: We haven't figured out static linking for libpg_query. Hence,
            # right now we can't build the node when building staticaly
            if static
            then [
              "deku-p/src/core/bin/api"
              "deku-p/src/core/bin/node"
              "deku-p/src/core/block_storage"
            ]
            else [];
        };

      # This is the same as standard dune build but with static support
      buildPhase = ''
        echo NODE_PATH
        echo $NODE_PATH
        runHook preBuild
        echo "running ${
          if static
          then "static"
          else "release"
        } build"
        dune build -p ${pname} --profile=${
          if static
          then "static"
          else "release"
        }
        runHook postBuild
      '';

      nativeBuildInputs = [nodejs removeReferencesTo] ++ npmPackages;

      checkPhase = ''
        runHook preInstall
        dune build -p ${pname} --profile=${
          if static
          then "static"
          else "release"
        } @runtest
        runHook postInstall
      '';

      propagatedBuildInputs =
        [
          tezos-micheline
          tezos-crypto
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
          eio
          eio_main
          caqti-eio
          routes
          ppx_rapper
          ppx_rapper_eio
          ezgzip
          ppx_jane # TODO: do we need this?
          core
          core_unix
        ]
        # checkInputs are here because when cross compiling dune needs test dependencies
        # but they are not available for the build phase. The issue can be seen by adding strictDeps = true;.
        ++ checkInputs;

      checkInputs = [alcotest];

      # Remove every directory which could have links to other store paths.
      # This makes the result much smaller
      isLibrary = false;
      postFixup =
        ''
          rm -rf $out/lib $out/share/doc
          remove-references-to \
            -t ${ocamlPackages.ocaml} \
            $out/bin/{deku-benchmark,deku-generate-identity,deku-helper,deku-node}
        ''
        + (
          if static
          then ''
            # If we're building statically linked binaries everything should be possible to remove
            remove-references-to \
              -t ${pkgs.gmp} \
              $out/bin/{deku-benchmark,deku-generate-identity,deku-helper,deku-node}
            remove-references-to \
              -t ${pkgs.libffi} \
              $out/bin/{deku-benchmark,deku-generate-identity,deku-helper,deku-node}
          ''
          else ""
        );

      meta.mainProgram = "deku-node";
    }
