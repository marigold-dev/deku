final: prev: {
  ocamlPackages = prev.ocaml-ng.ocamlPackages_5_00.overrideScope' (oself: osuper: {
    rely = osuper.reason-native.rely.overrideAttrs (_: {
      postPatch = ''
        substituteInPlace src/rely/TestSuiteRunner.re --replace "Pervasives" "Stdlib"
      '';
    });
    cmdliner_1_0_4 = osuper.cmdliner.overrideAttrs (_: {
      src = builtins.fetchurl {
        url = "https://github.com/dbuenzli/cmdliner/archive/refs/tags/v1.0.4.tar.gz";
        sha256 = "13c53b1cxkq2nj444655skw5a1mcxzbaqwqsqjf7jbwradb3hmxa";
      };
    });
  });
}
