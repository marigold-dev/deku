# Create your first JS Deku dApp

In this tutorial, we demonstrate how to create an DApp (using TypeScript) to interact with the Deku-Parametric or Deku-P in short.

TLDR;
Deku-P is a private blockchain (it has its own consensus, gossip, network, validators, etc.). Where it has a separate WASM act as the bridge, to communicate between your DApp and the blockchain. The advantage of Deku-P is that you only need to focus on writing your dApp, Deku-P will take care of how to handle the transaction, consensus, etc. for you.

## Prerequisites

0. You can clone the github directory and use the branch [parametric-develop](https://github.com/marigold-dev/deku/tree/parametric-develop), on your terminal type:

```
git clone https://github.com/marigold-dev/deku.git 
git checkout parametric-develop
```

You can follow this [readme](https://github.com/marigold-dev/deku/tree/parametric) on how to compile your project.

I will use `nix` to build this project as follow:

```
cd deku
direnv allow
dune build
```

1. We will use Typescript as a main language for our application. Create the folder for your new project inside the folder `examples`.  On your terminal type:

```
cd examples
mkdir tutorial
npm init
```

`npm init` to initialize your project.

2. Run `npm install typescript`, it will create a file `package.json`. 
Inside this file you can add the configuration file `tsconfig.json` which contain all the configurations for your typescript project.

```
"scripts": {
    "build": "tsc --p tsconfig.json",
    "test": "echo \"Error: no test specified\" && exit 1"
  },
```

Create a new file `tsconfig.json` at the same level of your typescript project

```
touch tsconfig.json
```

copy the content of this [`tsconfig.json`](https://github.com/marigold-dev/deku/blob/cookie-game/examples/cookie-game/tsconfig.json)


3. Now you need to build a WASM so that you have a communication channel between your application and the Deku-P. 

To do that in your `package.json` you can add the `"dependecies"` as follow:

```
 "dependencies": {
    "deku_js_interop": "file:../../sdks/deku_js_interop",
    "typescript": "^4.7.4"
  }
```

Or you can install `deku_js_interop` manually by using:

```
npm install "file:../.../sdks/deku_js_interop"
```

Here is the complete [`package.json`](https://github.com/marigold-dev/deku/blob/cookie-game/examples/cookie-game/package.json)


4. Now you can start to write your application. To do that create an `index.ts` file. This file will contain all the logic of your application.

```
touch index.ts
```

5. To be able to interact between Deku-P and your applcation, you need to have a Deku wallet. You can use the CLI `deku-cli` (for more information you can use `deku-cli --help`) provided by the Deku-P as follow:

```
alias deku-cli='nix run github:marigold-dev/deku/parametric#deku-cli --'
deku-cli create-wallet
```

This will generate a `tz1xxxx.tzsidewallet` file. Let's rename it to `wallet.json` for the example.

6. Finally, we can test the dApp with the Deku-P, in this example we are using the CLI provided by Deku-P `create-mock-transaction` to run the example without interact with the Deku-P cluster, to interact with Deku-P cluster use `create-custom-transaction`. 

```
npm i
deku-cli create-mock-transaction ./wallet.json $action node examples/tutorial/index.js
```

- `npm i`: build your TypeScript project.
- `$action`: is the input from dApp that you want to send to the WASM.

## Write dApp

Let's start to write the logic for our dApp. We want to print out the `Hello World`.

Inside `index.ts` we import the library `deku_js_interop` to guide our dApp on how to communicate with the WASM.


```typescript
import  { main, get, set, transaction } from "deku_js_interop"
```

- `main`: is the main function where it will take two parameters: 
  + initial state of your VM 
  + state transition when there is a new input
- `get`: retrieves the value from the local state, it will take a single parameter `key`, and will return the stored value.
- `set`: it will set a value  in a Deku state for a given key. It takes two parameters:
  + key: a key of the state
  + value: a value is a string encoded in json format
- `transition`: is a json received from the chain, it contains
```
interface transaction {
  source: string;
  tx_hash: string;
  op_hash: string;
  operation: { [key: string]: any };
}
```

The complete `index.ts` example:

```typescript
// @ts-ignore
import { main, get, set, transaction } from "deku_js_interop"

const transition = (tx: transaction) => {
    // get the current value of `state` field defined in `main` function
    let source_value = JSON.parse(get("my_state"));
    // set the new value to `Hello world!`
    source_value = "Hello World!";
    // save the new state
    set("my_state", source_value);
}

main({ my_state: "" }, transition);
```

- The `transition` function

```typescript
const transition = (tx: transaction) => {
}
```

The `tx` which is a `transaction` will allow you to retrieve and save the state of your application.


- The `main` function declare the initial state:
```typescript
main({ my_state: "" }, transition);
```

- `my_state: ""`: it is the initial state, it must be a `JSON` object with the `tz1xxx` address as key
- `transition`: it will update the state.

#### Run our dApp: Hello World

Call your VM transaction:

```
npm run build
deku-cli create-mock-transaction wallet.json '""' node examples/tutorial/index.js
```

- `npm run build` to build your VM.
- `$action` we give an empty string because we just want to call the `transition` with the value `Hello World` that already stored in our value out.

If you want to run on Deku-P cluster use:

```
tilt up -- --mode=local --vm="node examples/tutorial/index.js"
deku-cli create-custom-transaction data/0 wallet.json '""'
```

## Operation
In this example, instead giving the `$action` as an empty string, we will input the string `Hello World` from the dApp to the Deku-P cluster. To do that, let's modify the code in the function `transition` in `index.ts` as follow:

```typescript
const transition = (tx: transaction) => {
    console.log("Getting source");
    let source_value = JSON.parse(get("my_state"));
    console.log("Current value: " + source_value);
    // tx.operation is the last argument of `deku-cli` command line
    source_value = tx.operation;
    console.log("New value: " + source_value);
    set("my_state", source_value);
}
```

We will simply write it in the last argument of `deku-cli create-custom-transaction`:

```shell script
$ deku-cli create-custom-transaction data/0 wallet.json '"Hello world!"'
```

To change the state with another string `Something else` we can use:

```shell script
$ deku-cli create-custom-transaction data/0 wallet.json '"Something else"'
```

And check on tilt, the VM state updating:
```
Getting source
Current value:
New value: Hello world!
Getting source
Current value: Hello world!
New value: Something else
```

The complete source code of our tutorial can be find at: [tutorial](https://github.com/marigold-dev/deku/tree/cookie-game/examples/tutorial)

For more complex examples, you can have a look at [counter](https://github.com/marigold-dev/deku/tree/cookie-game/examples/ts-counter) or at [cookie-game](https://github.com/marigold-dev/deku/tree/cookie-game/examples/cookie-game).

## From the outside world

You are now able to modify your state from the outside world, let's do this with the HTTP APIs!

You can go further, and create an application using your state. For a complex example, you can have a look at [decookies](https://github.com/marigold-dev/decookies), which is a basic front UI using [cookie-game](../../examples/cookie-game/index.ts) state.

Your outside application will have to interact with the Deku VM. To do so, I will explain the different steps.

I will not focus about the front part, the only things that matters are the useful functions to:
1. retrieve the state
2. retrieve the height
3. create and submit the operation

### 1. Retrieve the actual state

On Deku side, there is the `POST /vm-state` endpoint:

```typescript
const get_actual_state = async () => {
  const state_request = await fetch("http://localhost:4440/vm-state",
    {
      method: "POST",
      body: JSON.stringify(null)
    });
  const state_response = await state_request.json();
  console.log(JSON.stringify(state_response));
  //should print
  //{"state":[["state","cookie"]]}
  return state_response;
}
```

The state is a list of pairs. For example, in the `decookies` project, the initia state is:
```json
{
    "tz1VULT8pu1NoWs7YPFWuvXSg3JSdGq55TXc":
    {
        cookie_baker_state:
        {
            number_of_cookie: 0,
            number_of_cursor: 0.,
            number_of_grandma: 0.,
            number_of_farm: 0.,
            number_of_free_cursor: 0,
            number_of_free_grandma: 0,
            number_of_free_farm: 0,
            cursor_cost: initial_cursor_cost,
            grandma_cost: initial_grandma_cost,
            farm_cost: initial_farm_cost,
            cursor_cps: 0,
            grandma_cps: 0,
            farm_cps: 0,
            total_cps: 0
        }
    }
}
```

Which is received as following from the `/vm-state` endoint:

```json
{
  "state": [
    [
      "tz1VULT8pu1NoWs7YPFWuvXSg3JSdGq55TXc",
      {
        "cookie_baker_state": {
          "number_of_cookie": 0,
          "number_of_cursor": 0,
          "number_of_grandma": 0,
          "number_of_farm": 0,
          "number_of_free_cursor": 0,
          "number_of_free_grandma": 0,
          "number_of_free_farm": 0,
          "cursor_cost": 15,
          "grandma_cost": 100,
          "farm_cost": 1100,
          "cursor_cps": 0,
          "grandma_cps": 0,
          "farm_cps": 0,
          "total_cps": 0
        }
      }
    ]
  ]
}
```

### 2. Retrieve the actual height

In order to submit an operation, you must retrieve the actual `block-level`, this can be done with:
```typescript
const request_block_level = async () => {
  const block_request = await fetch("http://localhost:4440/block-level",
    {
      method: "POST",
      body: JSON.stringify(null)
    });
  const block_response = await block_request.json();
  return block_response.level;
}
```

which response is:
```json
{ "level": 42 }
```

We will need it in the next section.

### 3. Call `/gossip-user-operation` endpoint


#### Create the payload

This endpoint must be call to submit your operation to Deku.
Here is an explained example of the body you should provide:

```json
{
  "user_operation": {
    "hash": "ac0a84c1f606dcaabbc4a16fdf83714c62bac34c9d56f55e32dc7fbe14270c83",
    "key": "edpkv5a9ZSXJDErjRC2N8hR4GbimYsQ45q646tsuBvCTDiVnpKU3Q9",
    "signature": "edsigtqWYS46iHWGJpgTgaG5KjsXmFoxoG9RvM1AWKk6wQm8PrN38YraPzjTqpMKXj1MQLKWrBXJ2Jknab3nzi65CdRg2fBBF6N",
    "nonce": 857314109,
    "block_height": 933,
    "data": {
      "hash": "d6d9a446af772944a4f477b29044d5c7acd2181e7eb4493342217fe788d23416",
      "source": "tz1VULT8pu1NoWs7YPFWuvXSg3JSdGq55TXc",
      "initial_operation": [
        "Vm_transaction",
        {
          "payload": "Hello World!"
        }
      ]
    }
  }
}
```

We will start explaining by the `data` nested object:
- `initial_operation`: is always a polymorphic array, whose first element is the string `"Vm_transaction"` and second element, a JSON record whre `payload` is the payload needed by our application, the last argument of the previous `deku-cli create-custom-transaction` command
```typescript
const initial_operation = ["Vm_transaction", {
    "Hello World!"
}];
```
- `source`: the `tz1xxxxx` address of the `wallet` submitting the operation
- `hash`: needs two different step:
```typescript
//1. stringify an array containing the tz1xxx address and the previously created initial_operation
const json_to_hash = JSON.stringify(["tz1xxxx", initial_operation]);
//2. 1. convert the previous string to hex
//2. 2. encode the hex using blake2b algorithm on base 58
//2. 3. decode the obtained string
//2. 4. slice it to remove prefix and suffix
const inner_hash = b58decode(encodeExpr(stringToHex(json_to_hash))).slice(4, -2);
```
- Finally, create the `data` JSON representation containing these three elements:
```typescript
const data = {
    hash: inner_hash, //⚠ respect the order of fields in the object for serialization
    source: "tz1xxx",
    initial_operation: initial_operation,
}
```

Now, let's move the other fields:
- `block_height` is the value you got on [step 2](./hello_world.md#2-retrieve-the-actual-height)
- `nonce` can basically be a random between 0 and the maximum Integer value
- `signature` can be obtained by using [taquito `signer`](https://tezostaquito.io/docs/inmemory_signer/) for the record type containing `nonce`, `block_height` and `data` previously created:

```typescript
const full_payload = JSON.stringify([ //FIXME: useless?
    nonce,
    block_height,
    data
]);
// Signer from taquito
const signature = await signer.sign(stringToHex(full_payload)).then((val) => val.prefixSig);
```
- And finally `hash`, is like before, the `hash` of the whole record (full_payload in this example):
```typescript
const hash = b58decode(encodeExpr(stringToHex(full_payload))).slice(4, -2);
```

#### Call the endpoint

Now we have the whole payload, we just need to wrap it in the revelant record to match the structure:
```typescript
// wrap the operation in a record type
const operation = {
      hash,
      key,
      signature,
      nonce,
      block_height,
      data
    }
// create a `user_operation` based on the previously created operation
const packet =
    { user_operation: operation };
// call the endpoint with the payload!
const result = await fetch("http://localhost:4440/gossip-user-operation",
    {
    method: "POST",
    body: JSON.stringify(packet)
    });
```

# 🎉 Voilà!! 👏 🥳

You just successfully updated the vm-state of you Deku dapp from a front-web!