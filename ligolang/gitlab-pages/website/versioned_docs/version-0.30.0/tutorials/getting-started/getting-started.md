---
id: getting-started
title: Getting started
---

This section is aimed at newcomers to Ligo and Tezos smart-contracts.
In this turorial, we will go through the following step :
-	Setting up the development environment,
-	Writing a simple contract in Cameligo
-	Testing the contract
-	Deploying the contract to tezos

# Setting up the development environment.
At the present moment, we recommand the user to develop on a UNIX system, GNU/Linux or MacOSX as the windows native binary is still in preparation. You can still use Ligo on windows through our docker image
More on [installation](https://ligolang.org/docs/intro/installation) and [editor support](https://ligolang.org/docs/intro/editor-support)

Alternatively, you can decide to use our [webide](https://ide.ligolang.org/). This can be usefull for testing or for small project. However, it doesn't scale well for bigger size projet as you won't be able to spread your project accross multiple files and use your own libraries.


## Install ligo

  If you are on linux, we have a `.deb` package ready for you. Those package are widely supported by linux distribution

  * On debian or unbuntun, download [the package](https://ligolang.org/deb/ligo.deb), and then install using: 
    ```zsh
    sudo apt install ./ligo.deb
    ```

  * If you are using another distribution, refer to their doc on how to install `.deb` packages.

  * Alternatively, you can download the program and install it by hand by running
    ```zsh
    wget https://ligolang.org/bin/linux/ligo
    chmod +x ./ligo
    ```
    Move it to you path for global install
    ```zsh
    sudo cp ./ligo /usr/local/bin
    ```
    If you choose this method, you will have to manually update the program by reproducing those step.




  Check that the installation is correct by opening a terminal and running the command
  ```zsh
  ligo --version
  ```
  If you get an error message, start again.
  If you don't, then let setup our editor.



## Setting up the editor 

  You can see the updated list of supported editor [here](https://ligolang.org/docs/intro/editor-support)


  In this tutorial, we will use vs-code.

  * For vs-code, simply go to the extension menu in the left bar (Ctrl + Shift + X) and search for the `ligo-vscode` extension and install it.

  * For emacs, follow the instruction [here](https://gitlab.com/ligolang/ligo/-/blob/dev/tools/emacs/README.md)

  * For vim, follow the instruction [here](https://gitlab.com/ligolang/ligo/-/blob/dev/tools/vim/ligo/start/ligo/README.md)

  Once, you've done it, you are ready to make your first smart-contract

## Install the tezos tools

  To deploy your smart-contract on the network and to test it, you will need to use a tezos client.

  * On GNU/linux, the simplest way to get tezos-client is through opam using `opam install tezos`. alternatives are avaliable [here](https://tezos.gitlab.io/introduction/howtoget.html)

  * On MacOsX, the sowtfare is distributed through a [brew](https://brew.sh/) formula with `brew install tezos`.

# Building a smart-contract.

In this section and the following one we will use a simple smart-contract that is present as example on our webide. We will cover the ligo language and smart-contract development in the following tutorials.

First, create a `ligo_tutorial` folder on your computer. Then download and put the contract in this folder. It is availiable in [Pascaligo](https://gitlab.com/ligolang/ligo/-/raw/dev/example/contracts/increment.ligo?inline=false), [Cameligo](https://gitlab.com/ligolang/ligo/-/raw/dev/example/contracts/increment.mligo?inline=false), [Reasonligo](https://gitlab.com/ligolang/ligo/-/raw/dev/example/contracts/increment.religo?inline=false) and [Jsligo](https://gitlab.com/ligolang/ligo/-/raw/dev/example/contracts/increment.jsligo?inline=false)

In the following, we consider that you are using the Cameligo contract, simply change the extension (`.mligo` for cameligo, `.ligo` for pascaligo, `.religo` for reasonligo, `.jsligo`) in case you use another one.

Open your editor in the folder and the file in the editor. you should have this code


<Syntax syntax="cameligo">

```ocaml
type storage = int

type parameter =
  Increment of int
| Decrement of int
| Reset

type return = operation list * storage

// Two entrypoints

let add (store, delta : storage * int) : storage = store + delta
let sub (store, delta : storage * int) : storage = store - delta

(* Main access point that dispatches to the entrypoints according to
   the smart contract parameter. *)
   
let main (action, store : parameter * storage) : return =
 ([] : operation list),    // No operations
 (match action with
   Increment (n) -> add (store, n)
 | Decrement (n) -> sub (store, n)
 | Reset         -> 0)
```
</Syntax>

Now we are going to compile the contract, open a terminal in the folder. (or the vs-code built-in terminal with  Ctrl+shift+²) and run the following command:

```zsh
ligo compile contract increment.mligo -o increment.tz
```

The `compile contract` take one parameters, the file you want to compile. The `-o` parameter indicates to store the result in increment.tz instead of outputting it in the terminal. By default, the `maiin` function will be use as entrypoint. To select another entrypoint use `-e`

Now, you should have a michelson contract `increment.tz` in the folder ready to be deploy. But before that, we want to test it to be sure that it behaves as expected, because once publish, it cannot be modified.

# Testing the contract

As we can never underline enough the importance of tests in the context of smart-contract. We will now test our contract three times on different levels :

## Test the code from the command line

  Using the `interpret` command, one can run ligo code in the context of an init file. For intances

  ```zsh
  ligo run interpret "<code>" --init-file increment.mligo
  ``` 

  will run `<code>` after evaluating everything in increment.mligo. This is usefull to test arbitrary function and variable in your code.

  For intance, to test the `add` function you can run
  ```zsh
  ligo run interpret "add(10,32)" --init-file increment.mligo
  ```
  which should return 42.
  Running several of this command will cover the complete code.

  To run the contract as called on the blockchain, you will prefer the command `dry-run` which take the contract, the entrypoint, the initial parameter and the initial storage, like so
  ```zsh
  ligo run dry-run increment.mligo "Increment(32)" "10"
  ```
  which will return (LIST_EMPTY(), 42).

  Combine several of those command to fully test the contract use-cases.


## Test the code with ligo test framework.

  In ligo, you are able to write test directly in the source file, using the test module. 

  Add the following line at the end of `increment.mligo`

<Syntax syntax="cameligo">

  ```ocaml
  let _test () =
    let initial_storage = 10 in
    let (taddr, _, _) = Test.originate main  initial_storage 0tez in
    let contr = Test.to_contract(taddr) in
    let _r = Test.transfer_to_contract_exn contr (Increment (32)) 1tez  in
    (Test.get_storage(taddr) = initial_storage + 32)

  let test = _test ()
  ```
</Syntax>

  which execute the same test as the previous section.

  Now simply run the command
  ```zsh
  ligo run test increment.mligo
  ```

  The command will run every function starting with `test` and return their values.

  More on the syntax for the test framework [here](https://ligolang.org/docs/advanced/testing#testing-with-test).


## Testing the michelson contract

  The ligo compiler is made so the produced michelson program types and correspond to the initial ligo program. However until we have tools for formal verification, we advise testing that the michelson code will behave as the ligo one. For this purpose, you should also write a test for the michelson code. 

  There is different methods for testing michelson code. In this tutorial we will focus on tezos-client mockup. More information [here](https://ligolang.org/docs/advanced/michelson_testing)

  This method consist in running a "mockup" tezos chain on our computer, push the contract on the chain and send transaction to the chain to test the contract behavior.

  First, create a temporary folder for the mockup chain by runnig 
  ```zsh
  mkdir /tmp/mockup
  ```

  Now start the node by running
  ```zsh
  tezos-client \
    --protocol PtEdoTezd3RHSC31mpxxo1npxFjoWWcFgQtxapi51Z8TLu6v6Uq \
    --base-dir /tmp/mockup \
    --mode mockup \
    create mockup
  ```

  This will run the node using the `Edo` protocol and return a few address, aliased from bootstrap1 to 5. For other version, check 
  `tezos-client list mockup protocols`

  You can now originate the contract to the mock net with :
  ```zsh
  tezos-client \                                                      
    --protocol PtEdo2ZkT9oKpimTah6x2embF25oss54njMuPzkJTEi5RqfdZFA \
    --base-dir /tmp/mockup \
    --mode mockup \
    originate contract mockup_testme \
                transferring 0 from bootstrap1 \
                running increment.tz \
                --init 10 --burn-cap 0.1
  ```
  you should see a lot of information on the command line and the information `New contract ... origninated`

  You can now start testing the contract.

  To check its storage run :
  ```zsh
  tezos-client \                                                      
    --protocol PtEdo2ZkT9oKpimTah6x2embF25oss54njMuPzkJTEi5RqfdZFA \
    --base-dir /tmp/mockup \
    --mode mockup \
    get contract storage for mockup_testme
  ```
  You should see a `10` in your terminal

  We are now ready to send a transaction to our contract. We want to send a transaction with parameter "Increment (32)" but the parameter is written is ligo.
  For that, it must first be converted to a michelson parameter. Which is done by running :

  ```zsh
  ligo compile parameter increment.mligo "Increment (32)"
  ```

  Which give you the result (Left (Right 32))

  Now we can send our transaction with the command

  ```zsh
  tezos-client \
    --protocol PtEdo2ZkT9oKpimTah6x2embF25oss54njMuPzkJTEi5RqfdZFA \
    --base-dir /tmp/mockup \
    --mode mockup \
  transfer 0 from bootstrap2 \
                to mockup_testme \
                --arg "(Left (Right 32))" --burn-cap 0.01
  ```
  The network will again send back many information including the updated storage which should now be equal to 42.

  This conclude our section about testing. As a exercice, you can write the test for the other entrypoint (decrease and reset).
  Once you are sure that the contract work corectly for all the use cases, you can move on to the next section

# Publishing the contract

For deploying the contract on tezos, we will use the `tezos-client` interface like we did on the previous section.

First, you will need an account address. You can get one for testing at the [faucet](https://faucet.tzalpha.net/).
Download the json file and place it in the `ligo_tutorial` folder. $!$ The account that you get from the faucet are only temporary

Then we are going to point the client on a tezos node
```zsh
tezos-client --endpoint https://testnet-tezos.giganode.io config update 
```
This is the testnet, which is a seperate network from Tezos, use for testing.


Once done, activate your account
```zsh
tezos-client activate account alice with <the name of the json file>
```

You will receive different messages from the node. The last one should confirm the activation of account Alice


You are now ready to originate your contract
```zsh
tezos-client originate contract increment \
              transferring 0 from alice \
              running increment.tz \
              --init 10 --burn-cap 0.1
```
Again, you will receive several messages from the node and you should get the confirmation that the contract has been published.

You can search your contract on the network using the portal [Better call dev](https://better-call.dev/)

You can know  call your contract with 
```zsh
tezos-client call increment from alice \
             --arg "(Left (Right 32))" \
             --burn-cap 0.1
```
If you do so, you will see several information on the operation, including the new contract storage.


This conclude this part of our tutorial.
You should now be able to compile, test, publish and call a contract.
Now you can go to the tacos shop tutorial to know more about programming with Ligo or you can start developping your own contract using the Ligo flavor you are more familiar with.
