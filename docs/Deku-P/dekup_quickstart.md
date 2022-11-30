---
sidebar_position: 1
---

# Quick-start with Deku-P

In this 20-minute tutorial, we'll get you up and running with Deku-P sidechains.
You'll learn how to:

- Write a Deku-P virtual machine in NodeJS
- Deploy a Deku-P network locally using Docker
- Interact with the network via the `deku-cli`

## Installing the Tools

For this example you'll need the following tools installed:

- [NodeJS](https://nodejs.org/en/download/) and npm.
- [Docker](https://docs.docker.com/engine/install/) and Docker Compose (ships with Docker in recent versions, check the [installation guide](https://docs.docker.com/compose/install/) for more info)
- The `deku-cli` (see footnote for OSX[^1]):
  ```bash
  wget https://github.com/marigold-dev/deku/releases/download/v0.1.0/deku-cli
  chmod +x ./deku-cli
  cp ./deku-cli /usr/bin
  ```

## Our First VM

Deku's consensus layer determines the order in which transactions are processed,
ensuring no two nodes ever commit to inconsistent histories. However, it
is up to you, the VM developer, to determine what these transactions mean.

A Deku VM is a program that receives transactions submitted by users and
responds with a series of writes and reads to the Deku data store using
the SDK. Deku VM's must be deterministic to ensure consistency across all nodes,
but beyond that there is no restriction on their behavior.

To get started, we'll first need the NodeJS SDK.

```bash
npm init -y
npm install @marigold-dev/deku-p-sdk
```

Let's make a VM that maintains a global counter that can incremented and
decremented. Create a file called `vm.js` and add the following:

```js
const { main, get, set } = require("@marigold-dev/deku-p-sdk");

const transition = (tx) => {
  // Parse the operation data
  const operation = JSON.parse(tx.operation);
  console.log("Parsed operation:", operation);
  const [operationKind, value] = operation;

  // Retrieve the current state of the 'counter' key.
  // This is an in-memory lookup - no IO required.
  const counter = JSON.parse(get("counter"));
  console.log("Current state:", counter);

  switch (operation[0]) {
    case "Increment":
      set("counter", JSON.stringify(counter + value));
      break;
    case "Decrement":
      set("counter", JSON.stringify(counter - value));
    default:
      const error = `Unrecognized operation kind: ${operationKind}`;
      console.error(error);
      // Signal an error to the user by returning a string
      return error;
  }
};

// Here we define the initial state of blockchain's key-value
// store on the genesis block.
const initialState = {
  counter: JSON.stringify(42),
};

main(initialState, transition);
```

### Testing the VM

During development we can use the `deku-cli` to test our VM.

First we'll need to create an identity for the user that will run the transaction:

```bash
deku-cli generate-identity --output ./wallet.json
```

Next, we can run a mock transaction against our vm. In this case we'll submit an
`Increment` operation:

```bash
deku-cli mock-transaction ./wallet.json '["Increment", 3]' 'node ./vm.js'
```

### Packaging Your Sidechain

To simplify running the chain locally, we'll package our sidechain with Docker.

First create a file called ./start.sh:

```bash
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

# Wait for Flextesa to start
sleep 5

# Start the deku-node in the background with the path
# to the pipe for communicating with the VM
deku-node --named-pipe-path /run/deku/pipe &

node ./vm.js /run/deku/pipe
```

Make ./start.sh executable:

```
chmod +x ./start.sh
```

Next, create a file called `Dockerfile` and add the following:

```dockerfile
# We'll use the official nodejs
# image as our base, but you should be able
# to use any image.
FROM node

# Copy the deku-node and it's required system libraries
# into your image.
COPY --from=ghcr.io/marigold-dev/deku:latest /nix /nix/
COPY --from=ghcr.io/marigold-dev/deku:latest /bin/deku-node /bin

WORKDIR /app

# Build your Deku VM
COPY package.json /app
RUN npm install
COPY ./vm.js .
COPY ./start.sh .
CMD /app/start.sh

```

Build and tag the image as `my-sidechain`:

```
docker build . -t my-sidechain
```

## Building a Network with Docker Compose

With the VM written and packaged, we can now declare a runnable Deku
network with Docker compose.

Here's an annotated configuration for a network of four Deku nodes.
Copy this into a file called `docker-compose.yml`:

```yaml
version: "3.6"
services:
  # We run a full Tezos sandbox network using Flextesa (https://tezos.gitlab.io/flextesa/).
  # The deku-flextesa image has already been pre-populated with a Deku bridge contract
  # configured for these nodes.
  flextesa:
    container_name: deku_flextesa
    # FIXME: publish this image somewhere
    image: ghcr.io/marigold-dev/deku-fextesa:latest
    command: kathmandubox start
    environment:
      - block_time=4
      - flextesa_node_cors_origin=*
    ports:
      - 127.0.0.1:20000:20000
  deku-node-0:
    container_name: deku-node-0
    image: my-sidechain
    environment:
      # A b58-encoded Ed25519 secret used to sign blocks
      - DEKU_SECRET=edsk4UWkJqpZrAm26qvJE8uY9ZFGFqQiFuBcDyEPASXeHxuD68WvvF
      - DEKU_PORT=4440
      # We'll enable the API for a single node on port 8080
      - DEKU_API_ENABLED=true
      - DEKU_API_PORT=8080
    network_mode: "host"
  deku-node-1:
    container_name: deku-node-1
    image: my-sidechain
    environment:
      - DEKU_SECRET=edsk2mbL2Z7bAmRnuYbmsRe8Yu9rgAq1h993SDxoZncmqyMHDECyBa
      - DEKU_PORT=4441
    network_mode: "host"
  deku-node-2:
    container_name: deku-node-2
    image: my-sidechain
    environment:
      - DEKU_SECRET=edsk3dx8ZfcaBXsuLsk8fawS1qxjHbZtEoEdpAwxhsjmYTQhoEUxFk
      - DEKU_PORT=4442
    network_mode: "host"
  deku-node-3:
    container_name: deku-node-3
    image: my-sidechain
    environment:
      - DEKU_SECRET=edsk3MwFfcGp5FsZgrX8FGiBiDutX2kfAuPzU6VdZpKYLyDRVPb879
      - DEKU_PORT=4443
    network_mode: "host"
```

You can now run your chain with `docker compose`:

```
docker compose up
```

:::caution
Shutting down the chain quickly during development can cause the chain
to hard-fork and get stuck. Use `docker compose up --force-recreate` until
[this is fixed](https://github.com/marigold-dev/deku/issues/911)
:::

## Interacting with the Chain

We can submit transactions with deku-cli using the wallet we created earlier:

```bash
deku-cli submit-transaction --api-uri http://localhost:8080 ./wallet.json '["Increment", 3]'
```

Once the operation is included, you can verify the result by querying the `counter` key of
blockchain state via the REST API:

```bash
curl http://localhost:8080/api/v1/state/unix/counter
```

For more about writing browser-based DApp frontends, out [@marigold-dev/deku](https://www.npmjs.com/package/@marigold-dev/deku) package on npm.
on this guide, or our blockchain game [Decookies](https://github.com/marigold-dev/deku/tree/main/decookies)
for an end-to-end example.

## Next Steps

With just a few lines of Javascript, you created an application-specific blockchain
distributed across 4 nodes.

However, blockchains are still intrinsically complex distributed systems, and there's lots
more to cover!

In the following articles, we'll deep dive Deku's architecture, operational characteristics,
and deployment best practices. We'll also cover some tips and tricks for a smooth
development experience when writing Deku-P applications.

[^1]: If you're on OSX, for now `deku-cli` must be built from source (see the [README](https://github.com/marigold-dev/deku#building-from-source)).
