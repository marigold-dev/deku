{ pkgs, ligo, deku, }:
pkgs.writeShellApplication {
  name = "sandbox.sh";
  runtimeInputs = with pkgs; [ deku jq curl ligo docker ];
  text = ''
    USE_NIX=y
    ${builtins.readFile ../sandbox.sh}
  '';
}
