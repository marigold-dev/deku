{ pkgs, deku, }:
pkgs.writeShellApplication {
  name = "sandbox.sh";
  runtimeInputs = with pkgs; [ deku jq curl ligo docker ];
  text = builtins.readFile ../sandbox.sh;
}
