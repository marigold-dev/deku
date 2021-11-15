# An overlay that adds ligo to ocamlPackages

{ sources ? import ./sources.nix
, CI_COMMIT_SHA ? builtins.getEnv "CI_COMMIT_SHA"
, COMMIT_DATE ? builtins.getEnv "COMMIT_DATE"
, CI_COMMIT_TAG ? builtins.getEnv "CI_COMMIT_TAG" }:
self: super:
let
  opam-nix = import sources.opam-nix (import sources.nixpkgs { });

  ocaml-overlay =
    import "${sources.tezos-packaging}/nix/build/ocaml-overlay.nix" {
      sources = import "${sources.tezos-packaging}/nix/nix/sources.nix";
    };

  fixHardeningWarning = pkg: if pkg.stdenv.isDarwin then pkg.overrideAttrs (_: {
    hardeningDisable = [ "strictoverflow" ];
  }) else pkg;

  inherit (import sources."gitignore.nix" { inherit (self) lib; })
    gitignoreSource;
  # Remove list of directories or files from source (to stop unneeded rebuilds)
  # Also, apply the gitignore here.
  filterOut = xs:
    gitignoreSource (self.lib.cleanSourceWith {
      filter = p: type: !(builtins.elem (builtins.baseNameOf p) xs);
      src = gitignoreSource ../.;
    });
in {
  ocamlPackages = (self.extend ocaml-overlay).ocamlPackages.overrideScope'
    (builtins.foldl' self.lib.composeExtensions (_: _: { }) [
      (opam-nix.callOPAMPackage (filterOut [
        ".git"
        ".gitlab-ci.yml"
        ".gitignore"
        "nix"
        "docker"
        "tools"
        "gitlab-pages"
      ]))
      (oself: osuper: {
        # Strange naming in nixpkgs
        ocamlfind = oself.findlib;
        lablgtk = null;

        # Strange problems
        bigstring = osuper.bigstring.overrideAttrs (_: { doCheck = false; });
        xmldiff = osuper.xmldiff.overrideAttrs (_: { src = sources.xmldiff; });
        # getopt = osuper.getopt.overrideAttrs (_: { configurePhase = "true"; });
        # Force certain versions
        ocaml-migrate-parsetree =
          osuper.ocaml-migrate-parsetree.versions."1.4.0";
        ppx_tools_versioned = osuper.ppx_tools_versioned.versions."5.2.3";
        bisect_ppx = osuper.bisect_ppx.versions."2.0.0".overrideAttrs (_: {
          src = builtins.fetchTarball
            "https://github.com/aantron/bisect_ppx/archive/02dfb10188033a26d07d23480c2bc44a3a670357.tar.gz";
        });
        coq = null;
        ocamlgraph = null ;
        hidapi = null ;
        lwt = null ;
        getopt = null ;


        hacl = fixHardeningWarning osuper.hacl;

        proto-alpha-utils = osuper.proto-alpha-utils.overrideAttrs (oa: rec {
          buildInputs = oa.buildInputs
            ++ [ oself.tezos-protocol-011-PtHangzH-parameters ];
          propagatedBuildInputs = buildInputs;
        });
        tezos-protocol-compiler = osuper.tezos-protocol-compiler.overrideAttrs
          (oa: rec {
            buildInputs = oa.buildInputs ++ [ oself.pprint ];
            propagatedBuildInputs = buildInputs;
          });

        # A combination of executables, libraries, documentation and test coverage
        ligo = self.buildEnv {
          name = "ligo";
          paths = with oself; [
            ligo-out.out
            ligo-tests
            ligo-doc
            ligo-coverage
          ];
        };

        ligo-dune = osuper.ligo.override { dune = osuper.dune_2; };
      })
    ]);
  coq = (self.coq_8_12.override { buildIde = false; csdp = null; });
}
