#! /bin/sh


join () {
  local IFS="$1"
  shift
  echo "$*"
}


data_directory="data"
validators=(0 1 2)
declare -a validator_secrets validator_secrets_quoted validator_secrets_quoted_array

echo "Creating validator identities. Use them to create the contract on Florencenet."
for i in ${validators[@]}
do
    mkdir -p "$data_directory/$validator"
    esy x sidecli generate-identity | jq . > "$data_directory/$validator/identity.json"
    validator_secrets[$i]=$(jq -r .public_key "$data_directory/$validator/identity.json")
done



validator_secrets_quoted=$(echo "${validator_secrets[*]}" | tr ' ' '\n' | awk 'BEGIN{OFS=",";}{ printf "(\"%s\":key) ", $0 }')
IFS=" "

validator_secrets_quoted_array=($validator_secrets_quoted)
	
validator_secrets_csv=$(join ';' "${validator_secrets_quoted_array[@]}")

echo "Paste the following in the storage section on the LIGO playground at https://ide.ligolang.org/p/-x6CdYJ5tIEaVzD9lGYsaA  and note the address of the contract"

cat <<EOF
{
  root_hash = {
    current_block_hash = 0x;
    current_block_height = 0;
    current_state_hash = 0x;
    current_handles_hash = 0x;
    current_validators = ([$validator_secrets_csv]: validators);
  };
  vault = {
    known_handles_hash = (Big_map.empty : vault_known_handles_hash_set);
    used_handles = (Big_map.empty : vault_used_handle_set);
    vault = (Big_map.empty : vault);
  }
}
EOF

# "KT1Q4G47qEuuo4U9QuXJ3WMVHBv3cwaFDPzG"
read -p "Enter address of the deployed contract: " contract_address
read -p "Enter secret key of wallet from https://faucet.tzalpha.net: " test_net_wallet_secret


for i in ${validators[@]}
do
    uri=$(jq -r .uri "$data_directory/$validator/identity.json")
    esy x sidecli setup-node "$data_directory/$validator" \
	--secret=$(jq -r .secret_key "$data_directory/$validator/identity.json") \
	--tezos_consensus_contract $contract_address \
	--tezos_rpc_node https://testnet-tezos.giganode.io \
	--tezos_secret $test_net_wallet_secret \
	--uri=$uri
done
