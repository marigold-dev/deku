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

copy the content of this [`tsconfig.json`](https://github.com/marigold-dev/deku/blob/cookie-game/examples/tutorial/tsconfig.json)


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

Here is the complete [`package.json`](https://github.com/marigold-dev/deku/blob/cookie-game/examples/tutorial/package.json)


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