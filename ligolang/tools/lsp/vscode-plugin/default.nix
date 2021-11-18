{ buildNpmPackage, ligo-squirrel, haskell-nix }:
buildNpmPackage {
  # use cleanGit from haskell.nix
  src = haskell-nix.haskellLib.cleanGit {
    name = "vscode-plugin";
    # location relative to git root
    src = ../../..;
    subDir = "tools/lsp/vscode-plugin";
  };

  npmBuild = ''
    mkdir bin
    cp -Lr ${ligo-squirrel}/* .
    npm run compile
    npm run package
    npm run lint
  '';

  installPhase = "mkdir $out; cp *.vsix $out";
}
