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
    mkdir -p "$data_directory/tmp/$i"
    esy x sidecli generate-identity --uri "http://localhost:444$i" | jq . > "$data_directory/tmp/$i/identity.json"
    validator_secrets[$i]=$(jq -r .public_key "$data_directory/tmp/$i/identity.json")
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
read -p "Download wallet from https://faucet.tzalpha.net and enter the path to the json file: " account_json_path

tezos-client -p "PsFLorenaUUuikDWvMDr6fGBRG8kt3e3D3fHoXK1j1BFRxeSH4i" --endpoint https://testnet-tezos.giganode.io import keys from mnemonic alice

tezos-client -p "PsFLorenaUUuikDWvMDr6fGBRG8kt3e3D3fHoXK1j1BFRxeSH4i" --endpoint https://testnet-tezos.giganode.io activate account alice with "$account_json_path"

for i in ${validators[@]}
do
    uri=$(jq -r .uri "$data_directory/tmp/$i/identity.json")
    esy x sidecli setup-node "$data_directory/$i" \
	--secret=$(jq -r .secret_key "$data_directory/tmp/$i/identity.json") \
	--tezos_consensus_contract $contract_address \
	--tezos_rpc_node https://testnet-tezos.giganode.io \
	--tezos_secret=$(jq -c '.[] | select(.name | contains("alice"))' ~/.tezos-client/secret_keys | jq -r .value | sed s/unencrypted://) \
	--uri=$uri 
done
