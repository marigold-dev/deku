final: prev:
let ocaml-lsp-5_00-src =
  prev.fetchFromGitHub
    {
      owner = "d4hines";
      repo = "ocaml-lsp";
      fetchSubmodules = true;
      rev = "3c761bfd82f636beff54b6eea1c704fe76610337";
      sha256 = "sha256-bLMVDZ9qE9PQbfyMELKWT5oVQm0ZOV+qGXLqBFqbHns=";
    }; in
with prev; {
  # ocaml-ng = builtins.mapAttrs (_: ocamlVersion: ocamlVersion) super.ocaml-ng;
  # TODO: this is clearly not right, I should be overriding only 4_14
  ocaml-ng = ocaml-ng // (with ocaml-ng; {
    ocamlPackages_5_00 = ocamlPackages_5_00.overrideScope'
      (oself: osuper: {
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
        alcotest = osuper.alcotest.overrideAttrs (o: {
          propagatedBuildInputs =
            prev.lib.lists.remove osuper.uuidm o.propagatedBuildInputs;
        });
        jsonrpc = osuper.jsonrpc.overrideAttrs (o: {
          src =
            if prev.lib.versionAtLeast oself.ocaml.version "5.00" then
              ocaml-lsp-5_00-src
            else
              o.src;
        });
      });
  });
}
