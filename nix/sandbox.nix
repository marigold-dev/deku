{ pkgs, ligo, deku, }: pkgs.writeShellApplication {
  name = "sandbox.sh";
  runtimeInputs = with pkgs; [ deku ligo jq curl docker ];
  text = builtins.readFile ../sandbox.sh;
}
