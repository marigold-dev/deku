---
sidebar_position: 2
---

import { ComponentCodeBlock } from "@theme/ComponentCodeBlock";

# Quick-start with Deku-C

In this 15-minute tutorial, we'll create our first DApp to Deku-C.

You'll learn how to:

- Write a simple smart contract in [Ligo](https://ligolang.org/).
- Compile and deploy it to Deku-C
- Interact with the contract via
  [`@marigold-dev/deku`](https://www.npmjs.com/package/@marigold-dev/deku)

## Installing the Tools

You can run deploy the contract in-browser without installing any additional
tools.

However, to follow along on along on the command line and to start developing
your own contracts, you'll need to install the LIGO compiler - check the
[installation instructions](https://ligolang.org/docs/intro/installation) for
your platform. This tutorial requires version 0.53.0 or higher.

You'll also need to install the Deku CLI:

```bash
npm install -g @marigold-dev/deku-cli
```

:::tip See the [Deku CLI Tutorial](./deku_c_cli.md) for more on using the CLI!
:::

## Our First Smart Contract

Let's write a simple counter, accepting the commands `Increment`, `Decrement`,
`Reset`.

```js
type storage = int;

type parameter = ["Increment", int] | ["Decrement", int] | ["Reset"];

type return_ = [list<operation>, storage];

const main = (action: parameter, store: storage): return_ => {
  let storage = match(action, {
    Increment: (n) => store + n,
    Decrement: (n) => store - n,
    Reset: () => 0,
  });
  return [list([]), storage];
};
```

Refer to the [Ligo documentation](https://ligolang.org/docs/intro/introduction)
for more on developing with Ligo, and don't hesitate to
[reach out](https://ligolang.org/contact)!

## Deploying Our Contract

We can originate our contract using `@marigold-dev/deku`, a Deku-C client
written in Typescript package. The client depends on
[Taquito](https://tezostaquito.io/) for signing interactions with Deku chain.
Taquito provides options for using a variety of browser-based and hardware
wallets, but for convenience we'll use the in-memory signer.

```js
import { DekuCClient } from "@marigold-dev/deku"
import { fromMemorySigner } from "@marigold-dev/deku"
import { InMemorySigner } from "@taquito/signer"

const memory = new InMemorySigner(
  "edsk3ym86W81aL2gfZ25WuWQrisJM5Vu8cEayCR6BGsRNgfRWos8mR"
);
const dekuSigner = fromMemorySigner(memory);

const dekuC = new DekuCClient({
  dekuRpc: "https://deku-canonical-vm0.deku-v1.marigold.dev/",
  ligoRpc: "https://ligo-deku-rpc.marigold.dev:9090"
  dekuSigner,
});
```

With a connection to Deku-C established, we're ready to deploy our contract! Try
running the example.

```js live noInline
const params = {
  kind: "jsligo",
  // The deku client will compile your jsligo code for you
  source: incrementLigoCode,
  // Give the initial storage as a Ligo expression string
  initialStorage: "1",
};

println(
  `Deploying contract with initial storage ${JSON.stringify(
    params.initialStorage
  )}...`
);
dekuC.originateLigo(params).then(({ operation, address }) => {
  println(`Operation successful! Operation hash: ${operation}`);
  println(`Deployment successful! New contract address: ${address}`);
});
```

<br/>

:::tip In addition to using `deku-toolkit` and/or command-line tools, you can
also develop Deku-C contract directly from your browser with the
[LIGO Playground](https://ide.ligolang.org/)!
:::

## Interacting with our Contract

Once deployed, we can use the Deku-C client to query and subscribe to our
contract's state, as well as invoke operations. In the live editor below, we've
hard-coded the address of a contract on Deku-C, but you can replace it with the
DK1 address of the contract you deployed above.

<!-- TODO: what happens when there are errors -->

```js live noInline
const code = { source: incrementLigoCode, kind: "jsligo" };
const myContract = dekuC.contract("DK14bVHNFE7QMQtQ8qdscz7w88RDsWoj7gqJ", code); // 👈 Replace with your contract address

println("Getting contract state...");
myContract
  .getState()
  .then((state) => println(`Current state is ${JSON.stringify(state)}`))
  .then(() => {
    // subscribe to changes
    myContract.onNewState((newState) =>
      println(
        `Contract state updated, next state is: ${JSON.stringify(newState)}`
      )
    );

    myContract.invokeLigo("Increment(3)");
  });
```
