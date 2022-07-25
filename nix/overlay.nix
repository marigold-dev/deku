self: super:
with super; {
  # ocaml-ng = builtins.mapAttrs (_: ocamlVersion: ocamlVersion) super.ocaml-ng;
  # TODO: this is clearly not right, I should be overriding only 4_14
  ocaml-ng = ocaml-ng // (with ocaml-ng; {
    ocamlPackages_4_14 = ocamlPackages_4_14.overrideScope'
      (_: super:
        let
          landmarks = super.buildDunePackage rec {
            pname = "landmarks";
            version = "1.4";
            src = builtins.fetchurl {
              url = "https://github.com/LexiFi/landmarks/archive/v${version}.tar.gz";
              sha256 = "0dnakz18lcgfd4pfjqjg6w5nh2qby45z0xp7d7qqgzlgj991b20d";
            };
            patches = [ ./landmarks-m1.patch ];
            propagatedBuildInputs = with super; [ ppxlib ];
          };
          landmarks-ppx = super.buildDunePackage {
            pname = "landmarks-ppx";
            inherit (landmarks) version src patches;
            propagatedBuildInputs = with super; [ ppxlib landmarks ];
          };
          ocaml-lsp = super.ocaml-lsp.overrideAttrs (_: {
            src = fetchFromGitHub {
              owner = "EduardoRFS";
              repo = "ocaml-lsp";
              rev = "eeb5ff15c6a3e78c897c6f787ce8ebb4ffe10934";
              sha256 = "+Ax6/fh0Kdlku1XWU2dukerGvvzznR4qjDsm9zG5cLg=";
              fetchSubmodules = true;
            };
          });
        in
        { inherit landmarks landmarks-ppx ocaml-lsp; });
  });
}
