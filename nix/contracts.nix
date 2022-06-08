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
}
