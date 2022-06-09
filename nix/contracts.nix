{ pkgs, ligo }:

{
  create-consensus-contract = pkgs.writeShellApplication {
    name = "create-consensus-contract.sh";
    runtimeInputs = with pkgs; [ ligo ];
    text = ''
      consensus_storage=$(${./scripts/create-consensus-storage.sh} "$@")
      consensus_contract="${../src/tezos_interop/consensus.mligo}"

      ligo compile storage "$consensus_contract" "$consensus_storage" > ./consensus_storage

      ligo compile contract "$consensus_contract" > ./consensus_contract
    '';
  };

  create-discovery-contract = pkgs.writeShellApplication {
    name = "create-discovery-contract.sh";
    runtimeInputs = with pkgs; [ ligo ];
    text = ''
      discovery_storage=$(${./scripts/create-discovery-storage.sh} "$@")
      discovery_contract="${../src/tezos_interop/discovery.mligo}"

      ligo compile storage "$discovery_contract" "$discovery_storage" > ./discovery_storage

      ligo compile contract "$discovery_contract" > ./discovery_contract
    '';
  };
}
