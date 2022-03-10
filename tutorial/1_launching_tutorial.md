# Launching a Deku Cluster


Recall that deploying a Deku testnet involves five steps:
1# To deploy a Deku testnet, we need to do several things:
1. Launch a Tezos testnet
2. Configure the Deku nodes that will run in the cluster such
    that they are all aware of each other
3. Start each node
4. Manually produce the first block
5. Manually sign the first block with 2/3rd's of the nodes

In our previous guide, we created did steps 1 and 2. The result
was three folders: `./data/0`, `./data/1`, and `./data/2` that contain the necessary configuration data for three Deku nodes to run
as a cluster.


In this guide, we'll do steps 3, 4, and 5.

(This guide assumes you configured a Deku cluster
using `./0_configuration_tutorial.sh`. If not, follow that tutorial now.)

## Step 3: Starting the nodes

We've created a simple docker-compose file to automate starting the three nodes.


```
cd ./tutorial
docker-compose up
```

For simplicity, we've added the `network_mode: "host"` option to each container so that
all services are available on localhost. For a more production you may wish to do differently.

## Step 4: Manually Producing the First Block

We'll produce the first block with node 0 (it could be any node).
We'll do this by executing a command inside node 0's docker container
with `docker exec`.

In a separate terminal from the one running `docker-compose`, run the following:
```
docker exec deku_node_0 /app/deku-cli produce-block /app/data
```

You will see a message like:
```
block.hash: <hash>
```

Copy the hash. We'll need it for the next step.

## Step 5: Manually Signing the First Block

We now need to manually sign the block hash with at least 2/3rds of the Deku cluster.
The node that produced the block (node 0 in our case) automatically signs the block
it produces, so it already has one signature. Because we are only running three nodes,
we only need one more signature. We'll get that signature from node 1.

```
HASH=<hash from step 4>
docker exec deku_node_1 /app/deku-cli sign-block /app/data $HASH
```

After this command, you should see the nodes producing blocks and commiting
hashes to the Tezos testnet. You can see the state of the consensus at any time
by opening the Better Call Dev gui on http://localhost:8000, navigating to the
consensus contract address, and inspecting the operations and storage.
