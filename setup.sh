#! /bin/sh


# join
# @param separator
# @param array of items to be joined with the separator
# @stdout string of items joined by separator
# @example join , "a" "b" -> "a,b"
join () {
  local IFS="$1"
  shift
  echo "$*"
}


data_directory="data"
validators=(0 1 2)
declare -a validator_secrets validator_secrets_quoted validator_secrets_quoted_array

# To get started, every node needs an identity. `esy x sidecli generate-identity --uri <uri>`
# will generate public and private keys to identify the node
# Below here, we do the same for 3 validator nodes.
echo "Creating validator identities. Use them to create the contract on Florencenet."
for i in ${validators[@]}
do
    mkdir -p "$data_directory/tmp/$i"
    esy x sidecli generate-identity --uri "http://localhost:444$i" | jq . > "$data_directory/tmp/$i/identity.json"
    validator_secrets[$i]=$(jq -r .public_key "$data_directory/tmp/$i/identity.json")
done

# With identities generated for the nodes, we collect the secret keys,
# and register them as authorised validators in the sidechain network.

# Feel free to skip the next few line - they just collect the secret
# keys from the identities created from the previous step.
validator_secrets_quoted=$(echo "${validator_secrets[*]}" | tr ' ' '\n' | awk 'BEGIN{OFS=",";}{ printf "(\"%s\":key) ", $0 }')
IFS=" "
validator_secrets_quoted_array=($validator_secrets_quoted)
validator_secrets_csv=$(join ';' "${validator_secrets_quoted_array[@]}")

# To register the validators, run consensus.mligo with the list of
# validators. To do this quickly, open the LIGO IDE with the url
# provided and paste the following storage as inputs to the contract.
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

# With the validators registered, use the address of the originated
# contract in the command to setup the individual nodes
read -p "Enter address of the deployed contract: " contract_address

# `esy x sidecli setup-node` can be used to setup the node. It takes
# 1. Data directory where the data and config files must be created
# 2. --tezos_consensus_contract takes the address obtained from
#    originating the smart contract at the LIGO IDE
# 3. --tezos_rpc_node takes the uri to a running Tezos node
# 4. --secret takes the secret key generated for each node from the
#    `esy x generate-identity` command run previously.
# 5. --tezos_secret
#    This secret key can be obtained from the test network from
#    faucet.tzalpha.net. While working with the test network, one
#    needs to import the `secret` from the activate code JSON downloaded
#    from [faucet.tzalpha.net](https://faucet.tzalpha.net) and activate
#    the address. 
read -p "Download wallet from https://faucet.tzalpha.net and enter the path to the json file: " account_json_path

# To import the keys obtained from faucet
tezos-client -p "PsFLorenaUUuikDWvMDr6fGBRG8kt3e3D3fHoXK1j1BFRxeSH4i" --endpoint https://testnet-tezos.giganode.io import keys from mnemonic alice

# To activate the address
tezos-client -p "PsFLorenaUUuikDWvMDr6fGBRG8kt3e3D3fHoXK1j1BFRxeSH4i" --endpoint https://testnet-tezos.giganode.io activate account alice with "$account_json_path"

# With all the inputs ready, we can now run the `setup-node` command
# for each node.
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
