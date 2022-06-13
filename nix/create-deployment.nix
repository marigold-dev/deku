{ pkgs, tezos-client, contracts }:

pkgs.writeShellApplication {
  name = "create-deku-deployment.sh";
  runtimeInputs = with pkgs; [ jq curl tezos-client ];
  text = ''
    function create-consensus-contract {
      # We are expecting it to split the words here so we disable shellcheck
      # shellcheck disable=2086
      ${contracts.create-consensus-contract}/bin/create-consensus-contract.sh "$@"
    }

    function create-discovery-contract {
      # We are expecting it to split the words here so we disable shellcheck
      # shellcheck disable=2086
      ${contracts.create-discovery-contract}/bin/create-discovery-contract.sh "$@"
    }

    ${builtins.readFile ./scripts/create-deku-deployment.sh}
  '';
}
