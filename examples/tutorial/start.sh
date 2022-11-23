#!/usr/bin/env bash

# A list of of the tz1 public key hashes for each validator in this network
# (derived from their secret keys)
export DEKU_VALIDATORS=tz1fpf9DffkGAnzT6UKMDoS4hZjNmoEKhGsK,tz1PYdVbnLwiqKo3fLFXTKxw6K7BhpddQPh8,tz1Pv4viWq7ye4R6cr9SKR3tXiZGvpK34SKi,tz1cXKCCxLwYCHDSrx9hfD5Qmbs4W8w2UKDw
export DEKU_VALIDATOR_URIS=localhost:4440,localhost:4441,localhost:4442,localhost:4443

export DEKU_TEZOS_RPC_NODE=http://flextesa:20000

# The secret key of a Tezos with which to post updates to Tezos. In Flextesa networks
# this wallet is pre-seeded with funds.
export DEKU_TEZOS_SECRET=edsk3QoqBuvdamxouPhin7swCvkQNgq4jP5KZPbwWNnwdZpSpJiEbq

# The address of the bridge contract deployed to the deku-flextesa Tezos network.
export DEKU_TEZOS_CONSENSUS_ADDRESS=KT1LHcxdRTgyFp1TdrgodVekLFkQwzFnTJcY

# During local development, it is sometimes useful to use smaller block sizes
# and to artificially throttle the block rate so as to not consume all the CPU's resources.
export DEKU_DEFAULT_BLOCK_SIZE=5000

# Enable Debug logging for the NodeJS SDK
export DEKU_VM_DEBUG_LOGGING=true
# Set the verbosity of the Deku node
export DEKU_LOG_VERBOSITY=info
export DEKU_API_LOG_VERBOSITY=info

# Wait for Flextesa to start
sleep 5

# Start the deku-node in the background with the path
# to the pipe for communicating with the VM
deku-node --named-pipe-path /run/deku/pipe &

node ./vm.js /run/deku/pipe
