{ sources ? import ./sources.nix }@args:
let pkgs = import ./pkgs.nix args;
in {
  inherit (pkgs)
    ligo-deb
    ligo-editor ligo-editor-docker
    ligo-website
    ligo-changelog;
}
