final: prev:
let disableCheck = package: package.overrideAttrs (o: { doCheck = false; });
in {
  ocaml-ng = builtins.mapAttrs (_: ocamlVersion:
    ocamlVersion.overrideScope' (oself: osuper: {
      lwt_react = osuper.lwt_react.overrideAttrs
        (o: { nativeBuildInputs = o.nativeBuildInputs ++ [ oself.cppo ]; });
      lwt_domain = osuper.buildDunePackage {
        pname = "lwt_domain";
        version = "0.2.0-dev";
        inherit (osuper.lwt) src;
        propagatedBuildInputs = with osuper; [ domainslib lwt ];
      };
      utop = osuper.utop.overrideAttrs (o: {
        propagatedBuildInputs = o.propagatedBuildInputs ++ [ oself.findlib ];
      });
      rely = osuper.reason-native.rely.overrideAttrs (_: {
        postPatch = ''
          substituteInPlace src/rely/TestSuiteRunner.re --replace "Pervasives" "Stdlib"
        '';
      });
      alcotest = osuper.alcotest.overrideAttrs (o: {
        propagatedBuildInputs =
          prev.lib.lists.remove osuper.uuidm o.propagatedBuildInputs;
      });

      # disable broken tests
      dream = disableCheck osuper.dream;
      data-encoding = disableCheck osuper.data-encoding;
      json-data-encoding = disableCheck osuper.json-data-encoding;
      json-data-encoding-bson = disableCheck osuper.json-data-encoding-bson;
      mrmime = disableCheck osuper.mrmime;
    })) prev.ocaml-ng;
}
