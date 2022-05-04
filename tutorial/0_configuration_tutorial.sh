#! /bin/bash
set -e

# Deku Testnet Tutorial

# To deploy a Deku testnet, we need to do several things:
# 1. Launch a Tezos testnet
# 2. Configure the Deku nodes that will run in the cluster such
#     that they are all aware of each other
# 3. Start each node
# 4. Manually produce the first block
# 5. Manually sign the first block with 2/3rd's of the nodes

# This script automates steps 1 and 2.
# It also acts a tutorial - we will explain the purpose of each command.

# First we'll build the docker container
docker build -f ../docker/Dockerfile -t deku ../

# To make logging easier for ourselves, we'll write a small logging function
message() {
  echo "=========== $@ ==========="
}

####### Step 1: Launching a Tezos testnet #########

# The first step is to launch a Tezos testnet. Additionally, we will launch
# the Better Call Dev Tezos block explorer for observability.
# We have automated this process with docker-compose
message "Launching Tezos testnet"
docker-compose -f ../tezos-testnet.docker-compose.yml up -d

###### Step 2: Configuring the Deku cluster #######

# Step 2 is by far the most complex of the 5, but we'll break it down
# step-by-step.


# In our Deku cluster we run 3 nodes, labeled 0, 1, and 2.

# Deku configuration requires several files. We will make folders to hold
# configuration for each node.
rm -rf ./data # Destroy any from old clusters first.
mkdir -p ./data/0
mkdir -p ./data/1
mkdir -p ./data/2


# We'll use docker to run the deku-cli
deku-cli() {
  docker run -v $PWD/data:/app/data deku /app/deku-cli "$@"
}

# Using the Deku cli, we will generate identities for each node.
# Importantly, we will decide the URI of each node with the '--uri' flag.
# For now we will use localhost and run on 3 different ports, but you could
# configure these to be whatever URI's you wish.
message "Creating Deku identities"
node0=$(deku-cli setup-identity --uri http://localhost:4440 ./data/0)
node1=$(deku-cli setup-identity --uri http://localhost:4441 ./data/1)
node2=$(deku-cli setup-identity --uri http://localhost:4442 ./data/2)

# Having obtained new identities, we will now configure and deploy a Deku consensus contract
# to the Tezos testnet.
# We must first generate the storage code to include our new Deku identities for $node0, $node1, and $node2
# We will generate the file using Bash Heredocs - the contents of the file are everything in between
# the two 'EOF' symbols.
message "Configuring Deku consenus smart contract"
consensus_storage=$(
  cat <<EOF
{
  root_hash = {
    current_block_hash = 0x;
    current_block_height = 0;
    current_state_hash = 0x;
    current_handles_hash = 0x;
    current_validators = [
        ("$node0": key_hash);
        ("$node1": key_hash);
        ("$node2": key_hash);
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

# Other nodes can be added later, but the existing nodes must sign to allow them to participate.

# The contract and its storage must be compiled with the ligo compiler.
# We will use the docker image of it.
ligo() {
  (cd .. && docker run --rm -v "$PWD":"$PWD" -w "$PWD" ligolang/ligo:0.28.0 "$@")
}

# Likewise, we need the tezos client to depoy the contract, but we will use the binary
# included in our Tezos testnet deployment.
tezos-client() {
  docker exec -it deku_flextesa tezos-client "$@"
}

# We'll first compile the Ligo contract.
message "Compiling Ligo contract and storage"
consensus="./src/tezos_interop/consensus.mligo"
consensus_storage=$(ligo compile storage "$consensus" "$consensus_storage")
contract=$(ligo compile contract $consensus)

# We'll use the Tezos node on localhost:20000 that is provided by our docker-compose file.
RPC_NODE=http://localhost:20000

# We'll need a Tezos wallet to deploy the contract with. For convenience, we'll
# use a hard-coded secret key to create this wallet.
message "Configuring Tezos client"
SECRET_KEY="edsk3QoqBuvdamxouPhin7swCvkQNgq4jP5KZPbwWNnwdZpSpJiEbq"
tezos-client --endpoint $RPC_NODE bootstrapped
tezos-client --endpoint $RPC_NODE config update
tezos-client --endpoint $RPC_NODE import secret key myWallet "unencrypted:$SECRET_KEY" --force

# Now we'll deploy the contract.
# launched with docker-compose.
message "Deploying contract"
tezos-client --endpoint $RPC_NODE originate contract "consensus" \
    transferring 0 from myWallet \
    running "$contract" \
    --init "$consensus_storage" \
    --burn-cap 2 \
    --force

# Next we'll look up the address of the contract we just deployed.
TEZOS_CONSENSUS_ADDRESS="$(tezos-client --endpoint $RPC_NODE show known contract consensus | grep KT1 | tr -d '\r')"
message "Tezos contract address is: $TEZOS_CONSENSUS_ADDRESS"

# We must also configure the nodes to be aware of each other. We do this initially by
# including a file called "validators.json" in their configuration folder
message "Creating validators.json"
validators_json=$(
  cat <<EOF
[
  {"address": "$node0", "uri": "http://localhost:4440"},
  {"address": "$node1", "uri": "http://localhost:4441"},
  {"address": "$node2", "uri": "http://localhost:4442"}
]
EOF
)

echo "$validators_json" > ./data/0/validators.json
echo "$validators_json" > ./data/1/validators.json
echo "$validators_json" > ./data/2/validators.json

# Likewise, node operators must manually allow for each change to the list
# of validators, otherwise the node will not sign a requested change.
# The file that configures this is called "trusted_validator_membership_change.json"
# We will generate that file now.
message "Creating trusted-membership-change.json"
trusted_validator_membership_change_json=$(
  cat <<EOF
 [
  {"action": ["Add"], "address": "$node0"},
  {"action": ["Add"], "address": "$node1"},
  {"action": ["Add"], "address": "$node2"}
] 
EOF
)

echo "$trusted_validator_membership_change_json" > ./data/0/trusted-validator-membership-change.json
echo "$trusted_validator_membership_change_json" > ./data/1/trusted-validator-membership-change.json
echo "$trusted_validator_membership_change_json" > ./data/2/trusted-validator-membership-change.json

# Lastly, we need to configure each Deku node to communicate with our Tezos testnet.
# This configuration is stored in a file called 'tezos.json', and is created with the
# Deku cli.

message "Configuring Deku-Tezos interop"

deku-cli setup-tezos ./data/0 \
      --tezos_consensus_contract="$TEZOS_CONSENSUS_ADDRESS" \
      --tezos_rpc_node=$RPC_NODE \
      --tezos_secret="$SECRET_KEY" \
      --unsafe_tezos_required_confirmations 1
  
deku-cli setup-tezos ./data/1 \
      --tezos_consensus_contract="$TEZOS_CONSENSUS_ADDRESS" \
      --tezos_rpc_node=$RPC_NODE \
      --tezos_secret="$SECRET_KEY" \
      --unsafe_tezos_required_confirmations 1

deku-cli setup-tezos ./data/2 \
      --tezos_consensus_contract="$TEZOS_CONSENSUS_ADDRESS" \
      --tezos_rpc_node=$RPC_NODE \
      --tezos_secret="$SECRET_KEY" \
      --unsafe_tezos_required_confirmations 1

# Note the usage of the option 'unsafe_tezos_required_confirmations'.
# This lowers the required confirmations 1, which means that Deku considers
# an operation finalized when just a single Tezos baker validates it.
# This is convenient for development, but not safe for production.

# We've now successfully configured our Deku cluster
message "Deku cluster ready to launch!"

# To start the chain, follow the instructions in the next tutorial: 1_launching_tutorial.md