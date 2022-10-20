---
sidebar_position: 1
---

# Quick-start with Deku-P

In this 20-minute tutorial, we'll get you up and running with Deku-P sidechains.
You'll learn how to:
- Write a Deku-P virtual machine in NodeJS 
- Deploy a Deku-P network locally using Docker
- Interact with the network via the `deku-cli`

## Installing the Tool

For this example you'll need the following tools installed:
- [NodeJS](https://nodejs.org/en/download/) and npm.
- [Docker](https://docs.docker.com/engine/install/) and Docker Compose (ships with Docker in recent versions, check the [installation guide](https://docs.docker.com/compose/install/) for more info)
- The `deku-cli` (see footnote[^1]).

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
            set("counter", JSON.stringify(counter + value))
            break;
        case "Decrement":
            set("counter", JSON.stringify(counter - value));
        default:
            const error = `Unrecognized operation kind: ${operationKind}`;
            console.error(error)
            // Signal an error to the user by returning a string
            return error
    }
}

// Here we define the initial state of blockchain's key-value
// store on the genesis block. 
const initialState = {
  counter: JSON.stringify(42)
}

main(initialState, transition);

```

### Testing the VM

During development we can use the `deku-cli` to test our VM.

First we'll need to create an identity for the user that will run the transaction:
```bash
deku-cli generate-identity ./wallet.json
```

Next, we can run a mock transaction against our vm. In this case we'll submit an
`Increment` operation:

```bash
deku-cli submit-transaction ./wallet.json '["Increment", 3]' 'node ./vm.js'
```

### Packaging the VM

To simplify running the chain locally, we'll package our VM with Docker.

Create a file called `Dockerfile` and add the following:
```dockerfile
FROM node
COPY ./vm.js .
CMD node ./vm.js
```

Build and tag the image as `my-vm`:
```
docker build . -t my-vm
```

## Building a Network with Docker Compose

With the VM written and packaged, we can now declare a runnable Deku
network with Docker compose.

Here's an annotated configuration for a network of four Deku nodes.
Copy this into a file called `docker-compose.yml`:

```yaml
version: "3.6"
volumes:
  # Each Deku node communicates with its own instance of our VM
  # via Unix named pipes. These volumes allow the docker processes
  # to both access the same pipes.
  node-0:
  node-1:
  node-2:
  node-3:
services:
  # We run a full Tezos sandbox network using Flextesa (https://tezos.gitlab.io/flextesa/).
  # The deku-flextesa image has already been pre-populated with a Deku bridge contract
  # configured for these nodes.
  flextesa:
    container_name: deku_flextesa
    # FIXME: publish this image somewhere
    image: ghcr.io/marigold-dev/deku-flextesa:latest
    command: kathmandubox start
    environment:
      - block_time=4
      - flextesa_node_cors_origin=*
    ports:
      - 127.0.0.1:20000:20000
    expose:
      - 20000/tcp
  deku-node-0:
    container_name: deku-node-0
    image: ghcr.io/marigold-dev/deku:latest
    volumes:
      - node-0:/run/deku
    ports:
      # Deku exposes an RPC on port 8080 by default. We'll expose that
      # port on a single node to allow our frontend to communicate with
      # the network.
      - 0.0.0.0:8080:8080
    expose:
      - 8080/tcp
      # By default Deku gossip is exchanged via TCP on port 4440.
      - 4440/tcp
    environment:
      # A b58-encoded Ed25519 secret used to sign blocks
      - DEKU_SECRET=edsk4UWkJqpZrAm26qvJE8uY9ZFGFqQiFuBcDyEPASXeHxuD68WvvF
      # A list of of the tz1 public key hashes for each validator in this network
      # (derived from their secret keys)
      - DEKU_VALIDATORS=tz1fpf9DffkGAnzT6UKMDoS4hZjNmoEKhGsK,tz1PYdVbnLwiqKo3fLFXTKxw6K7BhpddQPh8,tz1Pv4viWq7ye4R6cr9SKR3tXiZGvpK34SKi,tz1cXKCCxLwYCHDSrx9hfD5Qmbs4W8w2UKDw
      - DEKU_VALIDATOR_URIS=deku-node-1:4440,deku-node-2:4440,deku-node-3:4440,deku-node-3:4440
      - DEKU_TEZOS_RPC_NODE=http://flextesa:20000
      # The secret key of a Tezos with which to post updates to Tezos. In Flextesa networks
      # this wallet is pre-seeded with funds.
      - DEKU_TEZOS_SECRET=edsk3QoqBuvdamxouPhin7swCvkQNgq4jP5KZPbwWNnwdZpSpJiEbq
      # The address of the bridge contract deployed to the deku-flextesa Tezos network.
      - DEKU_TEZOS_CONSENSUS_ADDRESS=KT1LHcxdRTgyFp1TdrgodVekLFkQwzFnTJcY
      - DEKU_API_ENABLE=true
      # During local development, it is sometimes useful to use smaller block sizes
      # and to artificially throttle the block rate so as to not consume all the CPU's resources.
      - DEKU_DEFAULT_BLOCK_SIZE=1000
      - DEKU_MINIMUM_BLOCK_LATENCY=0.5
  deku-vm-0:
    image: my-vm
    container_name: deku-vm-0
    environment:
      - DEKU_VM_DEBUG_LOGGING=true
    volumes:
      # We connect our vm to the node via the docker volume 'node-0'.
      - node-0:/run/deku
  # The rest of the config is similar for each of the three other nodes.
  deku-node-1:
    container_name: deku-node-1
    image: ghcr.io/marigold-dev/deku:latest
    volumes:
      - node-1:/run/deku
    expose:
      - 4440/tcp 
    environment:
      # Node specific config
      - DEKU_SECRET=edsk2mbL2Z7bAmRnuYbmsRe8Yu9rgAq1h993SDxoZncmqyMHDECyBa
      # Common config
      - DEKU_VALIDATORS=tz1fpf9DffkGAnzT6UKMDoS4hZjNmoEKhGsK,tz1PYdVbnLwiqKo3fLFXTKxw6K7BhpddQPh8,tz1Pv4viWq7ye4R6cr9SKR3tXiZGvpK34SKi,tz1cXKCCxLwYCHDSrx9hfD5Qmbs4W8w2UKDw
      - DEKU_VALIDATOR_URIS=deku-node-1:4440,deku-node-2:4440,deku-node-3:4440,deku-node-3:4440
      - DEKU_TEZOS_RPC_NODE=http://flextesa:20000
      - DEKU_TEZOS_SECRET=edsk3QoqBuvdamxouPhin7swCvkQNgq4jP5KZPbwWNnwdZpSpJiEbq
      - DEKU_TEZOS_CONSENSUS_ADDRESS=KT1LHcxdRTgyFp1TdrgodVekLFkQwzFnTJcY
      - DEKU_API_ENABLE=true
      - DEKU_DEFAULT_BLOCK_SIZE=1000
      - DEKU_MINIMUM_BLOCK_LATENCY=0.5
  deku-vm-1:
    image: my-vm
    container_name: deku-vm-1
    volumes:
      - node-1:/run/deku
  deku-node-2:
    container_name: deku-node-2
    image: ghcr.io/marigold-dev/deku:latest
    volumes:
      - node-2:/run/deku
    expose:
      - 4440/tcp 
    environment:
      # Node specific config
      - DEKU_SECRET=edsk3dx8ZfcaBXsuLsk8fawS1qxjHbZtEoEdpAwxhsjmYTQhoEUxFk
      # Common config
      - DEKU_VALIDATORS=tz1fpf9DffkGAnzT6UKMDoS4hZjNmoEKhGsK,tz1PYdVbnLwiqKo3fLFXTKxw6K7BhpddQPh8,tz1Pv4viWq7ye4R6cr9SKR3tXiZGvpK34SKi,tz1cXKCCxLwYCHDSrx9hfD5Qmbs4W8w2UKDw
      - DEKU_VALIDATOR_URIS=deku-node-1:4440,deku-node-2:4440,deku-node-3:4440,deku-node-3:4440
      - DEKU_TEZOS_RPC_NODE=http://flextesa:20000
      - DEKU_TEZOS_SECRET=edsk3QoqBuvdamxouPhin7swCvkQNgq4jP5KZPbwWNnwdZpSpJiEbq
      - DEKU_TEZOS_CONSENSUS_ADDRESS=KT1LHcxdRTgyFp1TdrgodVekLFkQwzFnTJcY
      - DEKU_API_ENABLE=true
      - DEKU_DEFAULT_BLOCK_SIZE=1000
      - DEKU_MINIMUM_BLOCK_LATENCY=0.5
  deku-vm-2:
    image: my-vm
    container_name: deku-vm-2
    volumes:
      - node-2:/run/deku   
  deku-node-3:
    container_name: deku-node-3
    image: ghcr.io/marigold-dev/deku:latest
    volumes:
      - node-2:/run/deku
    expose:
      - 4440/tcp 
    environment:
      # Node specific config
      - DEKU_SECRET=edsk3MwFfcGp5FsZgrX8FGiBiDutX2kfAuPzU6VdZpKYLyDRVPb879
      # Common config
      - DEKU_VALIDATORS=tz1fpf9DffkGAnzT6UKMDoS4hZjNmoEKhGsK,tz1PYdVbnLwiqKo3fLFXTKxw6K7BhpddQPh8,tz1Pv4viWq7ye4R6cr9SKR3tXiZGvpK34SKi,tz1cXKCCxLwYCHDSrx9hfD5Qmbs4W8w2UKDw
      - DEKU_VALIDATOR_URIS=deku-node-1:4440,deku-node-2:4440,deku-node-3:4440,deku-node-3:4440
      - DEKU_TEZOS_RPC_NODE=http://flextesa:20000
      - DEKU_TEZOS_SECRET=edsk3QoqBuvdamxouPhin7swCvkQNgq4jP5KZPbwWNnwdZpSpJiEbq
      - DEKU_TEZOS_CONSENSUS_ADDRESS=KT1LHcxdRTgyFp1TdrgodVekLFkQwzFnTJcY
      - DEKU_API_ENABLE=true
      - DEKU_DEFAULT_BLOCK_SIZE=1000
      - DEKU_MINIMUM_BLOCK_LATENCY=0.5
  deku-vm-3:
    image: my-vm
    container_name: deku-vm-3
    volumes:
      - node-3:/run/deku   

```

You can now run your chain with `docker compose`:
```
docker compose up
```

## Interacting with the Chain

We can confirm the chain is working by submitting a transaction using
the wallet we created earlier:

```bash
deku-cli submit-transaction --api-uri http://localhost:8080 ./wallet.json '["Increment", 3]'
```

For writing browser-based DApp frontends, check out the [frontend tutorial](./frontend_tutorial.md) that builds
on this guide, or see our more detailed [examples](https://github.com/marigold-dev/deku/tree/main/examples).

## Next Steps

With just a few lines of Javascript, you created an application-specific blockchain
distributed across 4 nodes. 

However, blockchains are still intrinsically complex distributed systems, and there's lots
more to cover!

In the following articles, we'll deep dive Deku's architecture, operational characteristics,
and deployment best practices. We'll also cover some tips and tricks for a smooth
development experience when writing Deku-P applications.

[^1]: For now `deku-cli` must be built from source (see the [README](https://github.com/marigold-dev/deku#building-from-source)).
