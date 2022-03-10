#!/usr/bin/env bash

# As before, we'll use Docker to execute the Deku CLI
# For convenience, we've included a pre-made Deku wallet file. It consists of a public/private key pair.
# You can generate these yourself with the command 'deku-cli create-wallet'
deku-cli() {
  docker run \
    --net=host \
    -v $PWD/data:/app/data \
    -v $PWD/tz1bLjYA6EKKp7m2pkiMiB2ne5qMvSHQeZKm.tzsidewallet:/app/tz1bLjYA6EKKp7m2pkiMiB2ne5qMvSHQeZKm.tzsidewallet \
    deku /app/deku-cli "$@"
}

# We'll now run some transactions through the custom go state machine. We'll send them to different nodes
# simultaneously to test that they get processed in parallel correctly.
deku-cli create-transaction ./data/0 ./tz1bLjYA6EKKp7m2pkiMiB2ne5qMvSHQeZKm.tzsidewallet '{"Action":"Increment"}'
deku-cli create-transaction ./data/0 ./tz1bLjYA6EKKp7m2pkiMiB2ne5qMvSHQeZKm.tzsidewallet '{"Action":"Decrement"}' &
deku-cli create-transaction ./data/1 ./tz1bLjYA6EKKp7m2pkiMiB2ne5qMvSHQeZKm.tzsidewallet '{"Action":"Decrement"}' &
wait

# You could scale this test up to as many transactions as you wished.
