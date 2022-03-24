final: prev: {
  ocamlPackages = prev.ocaml-ng.ocamlPackages_5_00.overrideScope' (oself: osuper: rec {
    rely = osuper.reason-native.rely.overrideAttrs (_: rec {
      postPatch = ''
        substituteInPlace src/rely/TestSuiteRunner.re --replace "Pervasives" "Stdlib"
      '';
    });
    alcotest_1_5_0 = osuper.alcotest.overrideAttrs (_: {
      src = builtins.fetchurl {
        url = "https://github.com/mirage/alcotest/releases/download/1.5.0/alcotest-js-1.5.0.tbz";
        sha256 = "54281907e02d78995df246dc2e10ed182828294ad2059347a1e3a13354848f6c";
      };
      propagatedBuildInputs = with osuper; [ astring cmdliner_1_0_4 fmt uuidm re stdlib-shims uutf ];
      version = "1.5.0";
    });
    qcheck-alcotest = osuper.qcheck-alcotest.override {
      alcotest = alcotest_1_5_0;
    };
    cmdliner_1_0_4 = osuper.cmdliner.overrideAttrs (_: {
      src = builtins.fetchurl {
        url = "https://github.com/dbuenzli/cmdliner/archive/refs/tags/v1.0.4.tar.gz";
        sha256 = "13c53b1cxkq2nj444655skw5a1mcxzbaqwqsqjf7jbwradb3hmxa";
      };
    });
  });
}
