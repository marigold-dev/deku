final: prev:
let
  disableCheck = package: package.overrideAttrs (o: { doCheck = false; });
in
{
  ocaml-ng = builtins.mapAttrs
    (_: ocamlVersion:
      ocamlVersion.overrideScope' (oself: osuper: {
        # Ligo upstream isn't configured to suppport all our platforms, but it should build.
        # TODO: fix ligo platform support upstream
        lwt_react = osuper.lwt_react.overrideAttrs
          (o: { nativeBuildInputs = o.nativeBuildInputs ++ [ oself.cppo ]; });
        utop = osuper.utop.overrideAttrs (o: {
          propagatedBuildInputs = o.propagatedBuildInputs ++ [ oself.findlib ];
        });
        rely = osuper.reason-native.rely.overrideAttrs (_: {
          postPatch = ''
            substituteInPlace src/rely/TestSuiteRunner.re --replace "Pervasives" "Stdlib"
          '';
        });
        dream = osuper.dream.overrideAttrs (o: { doCheck = false; });
        alcotest = osuper.alcotest.overrideAttrs (o: {
          propagatedBuildInputs =
            prev.lib.lists.remove osuper.uuidm o.propagatedBuildInputs;
        });
        qcheck = osuper.qcheck.overrideAttrs (_: {
          src = prev.fetchurl {
            url = "https://github.com/c-cube/qcheck/archive/v0.18.1.tar.gz";
            sha256 = "1jzhwrzsf5290rs7hsa1my5yh1x95sh2sz92c4svd8yahzdlny7m";
          };
        });
        data-encoding = disableCheck osuper.data-encoding;
        json-data-encoding = disableCheck osuper.json-data-encoding;
        json-data-encoding-bson = disableCheck osuper.json-data-encoding-bson;
        hxd = disableCheck (osuper.hxd.overrideAttrs (o: {
          src = builtins.fetchurl {
            url =
              "https://github.com/dinosaure/hxd/releases/download/v0.3.1/hxd-v0.3.1.tbz";
            sha256 = "1g19dgwj29ykrv3gk7q66fjjlc1n1z9bz1y2q3g2klvww68nq8hw";
          };
        }));
        mrmime = disableCheck osuper.mrmime;
        cmdliner = osuper.cmdliner.overrideAttrs (_: {
          src = builtins.fetchurl {
            url =
              "https://github.com/dbuenzli/cmdliner/archive/refs/tags/v1.0.4.tar.gz";
            sha256 = "13c53b1cxkq2nj444655skw5a1mcxzbaqwqsqjf7jbwradb3hmxa";
          };
        });
        crowbar = osuper.crowbar.overrideAttrs (o:
          # TODO: I think this can be refactored away but I'm not sure how.
          let
            x = builtins.throw "foo";
            cmdliner = osuper.cmdliner.overrideAttrs (_: {
              src = builtins.fetchurl {
                url =
                  "https://github.com/dbuenzli/cmdliner/archive/refs/tags/v1.1.1.tar.gz";
                sha256 = "07846phk06hi90a764ijlrkv9xh69bdn2msi5ah6c43s8pcf7rnv";
              };
            }); in
          {
            propagatedBuildInputs = prev.lib.remove osuper.cmdliner o.propagatedBuildInputs ++ [ cmdliner ];
          });
        ligo = osuper.ligo.overrideAttrs (o: {
          meta = { platforms = oself.ocaml.meta.platforms; };
        });
      }))
    prev.ocaml-ng;
}
