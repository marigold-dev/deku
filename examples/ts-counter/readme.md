# Example: A Counter VM in Typescript

`example.ts` accepts increment or decrement transactions. With three simple steps:
  - reads a user defined counter from the Deku state
  - apply an increment or a decrement on it
  - commit the new counter to deku state

# How to install

You can install the example with:
```bash
$ npm --prefix "../../sdks/deku_js_interop" install
$ npm --prefix "../../sdks/deku_js_interop/" run build # To build the sdk
$ npm i # To install dependencies
$ npm run build # To build the typescript counter example
```

# How to test it ?

## Installing the Deku client

You can run this example with the `deku-cli`.
You can have it with the following alias (but you need nix):
```bash
alias deku-cli='nix run github:marigold-dev/deku/parametric#deku-cli --'
```
Or you can build the Deku project following the readme of deku. 
Then `deku-cli` will be located in `_build/install/default/bin/deku-cli`
If you build deku with nix, the deku client will be in your path.

## Test the example with a mock transaction

You can test the increment and decrement action by running a mock `Increment` transaction:

```bash
deku-cli create-mock-transaction ./wallet.json '"Increment"' node ./build/example.js
```

This will execute the VM with the given argument against its initial state, producing
output like the following:
```
fifo path: /tmp/deku_named_pipe_1521113be49393891d6f2273f0cc19fbd89ad6d25af7670a6889cd5116c9d727
opening read
opening write
vm started
sender: tz1cCUK2sqSmJCUNpoQUpy3Puy48H4Poic9s, tx_hash: b1a38e81b72f4a14ebb91bf65eecd08c2ac7e9ffaa97e54ea9f7533b2d492553
next counter value: 43
[["counter",43]]
```
