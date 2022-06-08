#! /usr/bin/env bash

set -e

consensus_storage=$(
  # Step 3: After having the Deku identities, we will configure and deploy
  # a Deku consensus contract to the Tezos testnet.
    cat <<EOF
  {
  root_hash = {
    current_block_hash = 0x;
    current_block_height = 0;
    current_state_hash = 0x;
    current_handles_hash = 0x;
    current_validators = [
EOF
    ## this iteration is done here just to ensure the indentation
    for ADDRESS in "$@"; do
      echo "      (\"$ADDRESS\": key_hash);"
    done
    cat <<EOF
    ];
  };
  vault = {
    known_handles_hash = (Big_map.empty : vault_known_handles_hash_set);
    used_handles = (Big_map.empty : vault_used_handle_set);
    vault = (Big_map.empty : vault);
  }
}
EOF

)

echo "$consensus_storage"