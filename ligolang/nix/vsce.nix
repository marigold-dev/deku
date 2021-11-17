{ sources ? import ./sources.nix }@args:
let pkgs = import ./pkgs.nix args;
    inherit (pkgs.callPackage sources.nix-npm-buildpackage { }) buildYarnPackage;
in (buildYarnPackage {
  src = sources.vscode-vsce;
  yarnBuild = ''
    yarn
    yarn run compile
  '';
  installPhase = ''
    cp -Lr . $out
    mkdir $out/bin
    ln -s $out/out/vsce $out/bin/vsce
  '';
})
