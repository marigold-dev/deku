---
sidebar_position: 2
---

import { ComponentCodeBlock } from "@theme/ComponentCodeBlock";

# Quick-start with Deku-C

> Under construction!

In this 15-minute tutorial, we'll create our first DApp to Deku-C.

You'll learn how to:

- Write a simple smart contract in [Ligo](https://ligolang.org/).
- Compile and deploy it to Deku-C
- Interact with the contract via [deku-c-toolkit](https://www.npmjs.com/package/@marigold-dev/deku-c-toolkit) npm package
- Create a DApp using React

## Installing the Tools

You can run deploy the contract in-browser without installing any additional
tools.

However, to follow along on along on the command line and to start developing
your own contracts, you'll need to install the LIGO compiler - check the
[installation instructions](https://ligolang.org/docs/intro/installation) for your
platform. This tutorial requires version 0.53 or higher.


You can also deploy contracts to Deku-C from the command line - see the
[Deku-C CLI Tutorial](./deku_c_cli.md) for more.

## Our First Smart Contract

Let's write a simple counter, accepting the commands `Increment`, `Decrement`,
`Reset`.


<!-- FIXME: not sure if this tutorial works since I switched to the jsLigo 52. -->
<!-- FIXME: tested with ligo 0.50 -->
<!-- TODO: test with ligo 0.54.1 -->

```js
type storage = int;

type parameter =
  | ["Increment", int]
  | ["Decrement", int]
  | ["Reset"];

type return_ = [list<operation>,storage];

const main = (action: parameter, store: storage): return_ => {
  let storage = match(action, {
    Increment: n => store + n,
    Decrement: n => store - n,
    Reset: () => 0
  });
  return [list([]), storage]
};
```

Refer to the [Ligo documentation](https://ligolang.org/docs/intro/introduction)
for more on developing with Ligo, and don't hesitate to
[reach out](https://ligolang.org/contact)!

## Compiling

Before a Ligo contract can be deployed to Deku-C, it must first be compiled to WebAssembly.

For example:

```bash
ligo compile contract ./increment.jsligo --wasm > increment.wat
```
In the case of our increment contract, this will produce the following
WebAssembly:

```wasm
 (module
  (import "env" "dup_host" (func $dup_host (param i64 ) (result)))
  (import "env" "pair" (func $pair (param i64 i64) (result i64)))
  ;; ..
  ;; lots more imports... truncated for brevity
  ;; ...
  (func $main (param $v1 i64) (result i64)
    (local $1 i64)
    (call $push (local.get $v1))
    (call $unpair (call $pop)) ;; implicit return
    (call $if_left (call $pop)) (if (then (call $if_left (call $pop)) (if (then (call $swap)
    (call $push (call $z_sub (call $pop) (call $pop)))) (else (call $push (call $z_add (call $pop) (call $pop)))))) (else (call $drop (i32.const 2))
    (call $push (call $zero)) (; 0 ;)))
    (call $push (call $nil))
    (call $push (call $pair (call $pop) (call $pop)))
    (call $pop))

  (export "push" (func $push))
  (export "pop" (func $push))
  (export "main" (func $main))
  (export "closures" (table $closures))
  (export "call_callback" (func $call_callback))
  (export "call_callback_unit" (func $call_callback_unit))
  ) 
```

## Deploying Our Contract

We can originate our contract using the Deku C Toolkit, a Typescript package for
interacting with deku canonical. The toolkit depends on [Taquito](https://tezostaquito.io/) for
signing interactions with Deku chain. Taquito provides options for using a variety
of browser-based and hardware wallets, but for convenience we'll use the in-memory signer.

```js
import { DekuCClient } from "@marigold-dev/deku-c-toolkit"
import { fromMemorySigner } from "@marigold-dev/deku-toolkit"
import { InMemorySigner } from "@taquito/signer"

const memory = new InMemorySigner(
  "edsk3ym86W81aL2gfZ25WuWQrisJM5Vu8cEayCR6BGsRNgfRWos8mR"
);
const signer = fromMemorySigner(memory);

const dekuC = new DekuCClient({
  dekuRpc: "https://deku-canonical-vm0.deku-v1.marigold.dev/",
  ligoRpc: "https://ligo.ghostnet.marigold.dev"
  signer,
});
```

With a connection to Deku-C established, we're ready to deploy our contract!
Try running the example.

```js live noInline
const params = {
  kind: "jsligo",
  initialStorage: 1,
  // The deku-c client will compile your jsligo code for you
  code: incrementLigoCode,
};

println(`Deploying contract with initial storage ${JSON.stringify(params.initialStorage)}...`);
dekuC.originateContract(params)
  .then(({operation, address}) => {
    println(`Operation successful! Operation hash: ${operation}`);
    println(`Deployment successful! New contract address: ${address}`);
  });
```
<br/>

:::tip
In addition to using `deku-toolkikt` and/or command-line tools, you can also develop and
deploy contracts to Deku-C directly from your browser with the [LIGO Playground](https://ide.ligolang.org/)!
:::

## Interacting with our Contract

Once deployed, we can query and subscribe to our contract's state with the Deku-C client.

```js
const myContract = dekuC.contract(window.myContractAddress);

const currentState = await myContract.getState();

myContract.subscribe((newState) => println(`Contract state updated, next state is: ${JSON.stringify(newtate)}`));
```

We can also invoke the contract like so:

```js
myContract.invoke(["Union", [ "Left", [ "Union", [ "Left", [ "Int", "3" ] ] ] ])
```

:::info
You can determine the contract parameters using the Ligo compiler:
```
ligo compile parameter ./increment.jsligo 'Increment (2)'  --wasm
```
:::

## Putting It All Together in a DApp

With all the parts assembled, we can write our first DApp.

Below is a simple react component providing a user interface to increment
contract that you can edit live. Replace `contractAddress` with your newly deployed
contract's address and try it out!


<ComponentCodeBlock code={`
  const MyFirstDApp = () => {
    const contractAddress = "DK1..."; // 👈 Replace with your contract address
    const myContract = dekuC.contract(contractAddress);
    const [counter, setCounter] = useState(1); // The initial value can be your initial state
    const [delta, setDelta] = useState(1);
    const handleInputChange = (event) => setDelta(event.target.value);
    const handleIncrement = () => myContract.invoke("Increment", int(delta));
    const handleDecrement = () => myContract.invoke("Decrement", int(delta));
    const handleReset = () => myContract.invoke("Reset");
    useEffect(() => {
      myContract.onNewState(newState => setCounter(newState));
    }, []);
    return (
      <div>
        <h3>Connected to contract: {contractAddress}</h3>
        <b><p>Current Counter State: {counter}</p></b>
        <p><input value={delta} onChange={handleInputChange}/></p>
        <button onClick={handleDecrement}>Increment</button>
        <button onClick={handleIncrement}>Decrement</button>
        <button onClick={handleReset}>Reset</button>
      </div>
    );
  };
  render(<MyFirstDApp/>)
`}>
</ComponentCodeBlock>