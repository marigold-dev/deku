final: prev:
let disableCheck = package: package.overrideAttrs (o: { doCheck = false; });
in {
  ocaml-ng = builtins.mapAttrs (_: ocamlVersion:
    ocamlVersion.overrideScope' (oself: osuper: rec {
      rely = osuper.reason-native.rely.overrideAttrs (_: {
        postPatch = ''
          substituteInPlace src/rely/TestSuiteRunner.re --replace "Pervasives" "Stdlib"
        '';
      });
      dream = osuper.dream.overrideAttrs (o: {
        doCheck = false;
        propagatedBuildInputs = o.propagatedBuildInputs
          ++ [ osuper.ppx_inline_test ];
      });
      alcotest = osuper.alcotest.overrideAttrs (o: {
        src = prev.fetchurl {
          url =
            "https://github.com/mirage/alcotest/releases/download/1.5.0/alcotest-js-1.5.0.tbz";
          sha256 = "0v4ghia378g3l53r61fj98ljha0qxl82xp26y9frjy1dw03ija2l";
        };
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
      odoc-parser = osuper.odoc-parser.overrideAttrs (o: {
        propagatedBuildInputs = o.propagatedBuildInputs
          ++ (with oself; [ camlp-streams result astring ]);
      });
    })) prev.ocaml-ng;
}
