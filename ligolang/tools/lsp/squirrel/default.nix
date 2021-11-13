# SPDX-FileCopyrightText: 2020 Serokell <https://serokell.io>
#
# SPDX-License-Identifier: MPL-2.0

{ haskell-nix, grammars, runCommand, hpack }:
let
  projectSrc = haskell-nix.haskellLib.cleanGit {
    name = "squirrel";
    # location relative to git root
    src = ../../..;
    subDir = "tools/lsp/squirrel";
  };

  # haskell.nix can generate .cabal file automatically, but it uses a custom
  # build of hpack which requires rebuilding GHC and all haskell dependencies.
  # We use hpack from nixpkgs instead to avoid big rebuilds.
  cabalFile = runCommand "ligo-squirrel.cabal" {} ''
    ${hpack}/bin/hpack ${projectSrc} - > $out
  '';

  project = haskell-nix.stackProject {
    # project src with .cabal file added
    src = runCommand "src-with-cabal" {} ''
      cp -r --no-preserve=mode ${projectSrc} $out
      cp ${cabalFile} $out/ligo-squirrel.cabal
    '';

    ignorePackageYaml = true;

    modules = [
      ({ config, ... }: {
        packages.ligo-squirrel = {
          preBuild = ''
            rm -rf grammar
            cp -r ${grammars} grammar
          '';

          # Thanks, I Hate It.
          components.tests.ligo-contracts-test = {
            preBuild = "export CONTRACTS_DIR=${../../../src/test/contracts}";
          };

          ghcOptions = ["-Werror"];

          # strip library references from the executable to reduce closure size
          dontStrip = false;
        };
      })
    ];
  };
in project.ligo-squirrel
