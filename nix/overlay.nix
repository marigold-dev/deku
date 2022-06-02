final: prev:
let disableCheck = package: package.overrideAttrs (o: { doCheck = false; });
in {
  pollinate = disableCheck prev.pollinate;
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
      jsonrpc = osuper.jsonrpc.overrideAttrs (o: {
        src = if prev.lib.versionAtLeast oself.ocaml.version "5.00" then
          prev.fetchFromGitHub {
            owner = "ulrikstrid";
            repo = "ocaml-lsp";
            fetchSubmodules = true;
            rev = "191f65ab82efc56c370e9e3122123590b96071fd";
            sha256 = "sha256-FqQzh+SvRmZ6xTdcyr0iF3EE+8o+I9LSUJ5FgI5UyoU=";
          }
        else
          o.src;
      });

      # disable broken tests
      dream = disableCheck osuper.dream;
      data-encoding = disableCheck osuper.data-encoding;
      json-data-encoding = disableCheck osuper.json-data-encoding;
      json-data-encoding-bson = disableCheck osuper.json-data-encoding-bson;
      mrmime = disableCheck osuper.mrmime;
    })) prev.ocaml-ng;
}
