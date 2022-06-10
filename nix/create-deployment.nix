{ pkgs, tezos-client }:

pkgs.writeShellApplication {
  name = "create-deku-deployment.sh";
  runtimeInputs = with pkgs; [ jq curl tezos-client ];
  text = "${builtins.readFile ./scripts/create-deku-deployment.sh}";
}