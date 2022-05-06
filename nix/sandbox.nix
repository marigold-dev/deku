{ pkgs, deku, }:
pkgs.writeShellApplication {
  name = "sandbox.sh";
  runtimeInputs = with pkgs; [ deku jq curl docker ];
  text = builtins.readFile ../sandbox.sh;
}
