# How to do a deposit and withdraw on deku

Here is the scenario of this end to end test:

1. Start Deku
2. Originate a ticket contract
3. Deposit ticket on Deku
4. Transfer to Alice
5. Withdraw as Alice

## Prerequisites: start Deku cluster

```shell script
$ docker compose up -d #To start flextesa (sandbox tezos node)
$ ./sandbox.sh setup
$ ./sandbox.sh start #blocks should appear
```

## Deploy a contract holding tickets

Creates a contract which can deposit or withdraw tickets from the consensus contract:

```shell script
$ ./sandbox.sh deploy-dummy-ticket

=========== Running in local mode ===========
Node is bootstrapped.
Estimated gas: 1428.723 units (will add 100 for safety)
Estimated storage: 547 bytes added (will add 20 for safety)
Operation successfully injected in the node.
Operation hash is 'opSjxW4yLmkqyYkU4ZBdk2xvf28z7AfLtyb3z19Q1UDQDu1KTzC'
Waiting for the operation to be included...
Operation found in block: BM8vszyYJLsk1JdZahN9RmPD3JWNJ7fqu3gn3QR6GU1v9zA3dan (pass: 3, offset: 0)
This sequence of operations was run:
  Manager signed operations:
    From: tz1VSUr8wwNhLAzempoch5d6hLRiTh8Cjcjb
    Fee to the baker: ꜩ0.00067
    Expected counter: 6
    Gas limit: 1529
    Storage limit: 567 bytes
    Balance updates:
      tz1VSUr8wwNhLAzempoch5d6hLRiTh8Cjcjb ........... -ꜩ0.00067
      fees(tz1YPSCGWXwBdTncK2aCctSZAXWvGsGwVJqU,3) ... +ꜩ0.00067
    Origination:
      From: tz1VSUr8wwNhLAzempoch5d6hLRiTh8Cjcjb
      Credit: ꜩ0
      Script:
        { parameter
            (or (pair %deposit
                   (pair (bytes %bytes) (address %deku_consensus))
                   (address %deku_recipient))
                (ticket %withdraw bytes)) ;
          storage (list (ticket bytes)) ;
          code { UNPAIR ;
                 IF_LEFT
                   { UNPAIR ;
                     UNPAIR ;
                     SWAP ;
                     CONTRACT %deposit (pair (address %address) (ticket %ticket bytes)) ;
                     IF_NONE
                       { DROP 3 ; PUSH string "Entrypoint doesn't exist" ; FAILWITH }
                       { PUSH mutez 0 ;
                         PUSH nat 1000000 ;
                         DIG 3 ;
                         TICKET ;
                         DIG 3 ;
                         PAIR ;
                         TRANSFER_TOKENS ;
                         SWAP ;
                         NIL operation ;
                         DIG 2 ;
                         CONS ;
                         PAIR } }
                   { CONS ; NIL operation ; PAIR } } }
        Initial storage: {}
        No delegate for this contract
        This origination was successfully applied
        Originated contracts:
          KT1WyREzcYATwbcbGPnLvDvUAHQcY7SJLm82
        Storage size: 290 bytes
        Paid storage size diff: 290 bytes
        Consumed gas: 1428.723
        Balance updates:
          tz1VSUr8wwNhLAzempoch5d6hLRiTh8Cjcjb ... -ꜩ0.0725
          tz1VSUr8wwNhLAzempoch5d6hLRiTh8Cjcjb ... -ꜩ0.06425

New contract KT1WyREzcYATwbcbGPnLvDvUAHQcY7SJLm82 originated.
The operation has only been included 0 blocks ago.
We recommend to wait more.
Use command
  tezos-client wait for opSjxW4yLmkqyYkU4ZBdk2xvf28z7AfLtyb3z19Q1UDQDu1KTzC to be included --confirmations 5 --branch BLh4dN47edk4cpSgiTBU8bhhVYSVj48mgu51sHbPDKLv8wVNFDt
and/or an external block explorer.
Contract memorized as dummy_ticket.
```

Save the contract address `KT1WyREzcYATwbcbGPnLvDvUAHQcY7SJLm82` from the previous command output:

> [...]
> New contract KT1WyREzcYATwbcbGPnLvDvUAHQcY7SJLm82 originated.
> [...]

_From now, we will replace `KT1WyREzcYATwbcbGPnLvDvUAHQcY7SJLm82` by `CONTRACT_ADDRESS`._

## Deposit tickets from your contract to Deku

Creates `n` tickets (10000000 defined in the contract) and deposit them in the consensus contract so they are available in Deku for the address `tz1RPNjHPWuM8ryS5LDttkHdM321t85dSqaf` (Deku address hard coded in the `sandbox.sh` script)

_From now, we will replace `tz1RPNjHPWuM8ryS5LDttkHdM321t85dSqaf` by `DEKU_ADDRESS`._

```shell script
$ ./sandbox.sh deposit-dummy-ticket

=========== Running in local mode ===========
Node is bootstrapped.
Estimated gas: 4624.858 units (will add 100 for safety)
Estimated storage: 105 bytes added (will add 20 for safety)
Operation successfully injected in the node.
Operation hash is 'ooFnJgW5jUsFuYFxt4ip3mXEcjjdytHUgaCUGe3PvvEhiMzoa65'
Waiting for the operation to be included...
Operation found in block: BKphQACo4G2y3hTXpKePWXF8sB7MEQJVmfWcU1VmURcvwa18f33 (pass: 3, offset: 0)
This sequence of operations was run:
  Manager signed operations:
    From: tz1VSUr8wwNhLAzempoch5d6hLRiTh8Cjcjb
    Fee to the baker: ꜩ0.000825
    Expected counter: 12
    Gas limit: 4725
    Storage limit: 125 bytes
    Balance updates:
      tz1VSUr8wwNhLAzempoch5d6hLRiTh8Cjcjb ........... -ꜩ0.000825
      fees(tz1YPSCGWXwBdTncK2aCctSZAXWvGsGwVJqU,4) ... +ꜩ0.000825
    Transaction:
      Amount: ꜩ0
      From: tz1VSUr8wwNhLAzempoch5d6hLRiTh8Cjcjb
      To: CONTRACT_ADDRESS
      Entrypoint: deposit
      Parameter: (Pair (Pair 0x "KT1DNb3Hap4DwkpVmiYxH7UmcjmZANtQLEFv")
                       "DEKU_ADDRESS")
      This transaction was successfully applied
      Updated storage: {}
      Storage size: 290 bytes
      Consumed gas: 2808.124
    Internal operations:
      Transaction:
        Amount: ꜩ0
        From: CONTRACT_ADDRESS
        To: KT1DNb3Hap4DwkpVmiYxH7UmcjmZANtQLEFv
        Entrypoint: deposit
        Parameter: (Pair 0x00003f0b3dd85deb3013c6a239fe9e3783ace4f8e37c
                         (Pair 0x01f59b5e7986cec18cfc99b3a9c4a601b73dbdea1c00 (Pair 0x 1000000)))
        This transaction was successfully applied
        Updated storage:
          (Pair (Pair (Pair (Pair 0xdf289a57e28aed1e42f84e65bb2c9e897746a16acb2f297295ecf3823ee37cc0 97)
                            (Pair 0x0e5751c026e543b2e8ab2eb06099daa1d1e5df47778f7787faab45cdf12fe3a8
                                  0x8e18383c67c247b222b328d73690f939b3ea05b5644a6f6f3bb460e0c463af68))
                      { 0x00031958f137649fc62902c090e042c1fe03270b13 ;
                        0x0012b1a66c13bce550ce285f8085506fca460544bc ;
                        0x00021a0ead6b4e615981eac146b375f9040ef3c600 ;
                        0x00f752ef4b5df8da5fef86fb5065cbc3eb241324f9 })
                (Pair (Pair 4 5) 6))
        Updated big_maps:
          Set map(6)[(Pair 0x01f59b5e7986cec18cfc99b3a9c4a601b73dbdea1c00 0x)] to (Pair 0x01f59b5e7986cec18cfc99b3a9c4a601b73dbdea1c00 (Pair 0x 1000000))
        Storage size: 4649 bytes
        Paid storage size diff: 105 bytes
        Consumed gas: 1816.734
        Balance updates:
          tz1VSUr8wwNhLAzempoch5d6hLRiTh8Cjcjb ... -ꜩ0.02625

The operation has only been included 0 blocks ago.
We recommend to wait more.
Use command
  tezos-client wait for ooFnJgW5jUsFuYFxt4ip3mXEcjjdytHUgaCUGe3PvvEhiMzoa65 to be included --confirmations 5 --branch BM5WddbfGLMEwq26c8sF9u5qCsWqyfNAisMiQaYUgncEatnB5Mq
and/or an external block explorer.
```

## Deku wallet creation

Create an address on Deku for Alice:

```shell script
$ deku-cli create-wallet
```

Save the field `address` and rename the generated `*.tzsidewallet` file to `alice.wallet`.

Create a wallet for the ticket holder named `wallet.json` with:

```JSON
{
  "address": "DEKU_ADDRESS",
  "priv_key": "edsk36FhrZwFVKpkdmouNmcwkAJ9XgSnE5TFHA7MqnmZ93iczDhQLK"
}
```

Where:
- `DEKU_ADDRESS` is the Deku address _(from `sandbox.sh` script)_
- `edsk36FhrZwFVKpkdmouNmcwkAJ9XgSnE5TFHA7MqnmZ93iczDhQLK` is the secret _(from `sandbox.sh` script)_.

## Transfer tickets to Alice

Create a transaction to transfer 10 tickets from `wallet.json` to Alice:

```shell script
$ deku-cli create-transaction data/0 wallet.json "tz1N781N76jRmVZgzZFgoNfyi77rw6f7WoDa" 10 "(Pair \"CONTRACT_ADDRESS\" 0x)"

operation.hash: a4ebc04591f2806045ede1dbc4be9b4a4eff503be69cc970132a2ec1ec7f05db
```

where:
- `tz1N781N76jRmVZgzZFgoNfyi77rw6f7WoDa` is the public address of Alice
- `CONTRACT_ADDRESS` is the dummy ticket contract address (see [deploy a contract holding tickets](./deposit-withdraw.md#Deploy-a-contract-holding-tickets))

Now Alice has 10 tickets!

_From now, we will replace `tz1N781N76jRmVZgzZFgoNfyi77rw6f7WoDa` by `ALICE_PUBLIC_ADDRESS`._

### Known error

If you have this following error:

```shell script
deku-cli: internal error, uncaught exception:
          Node.Networking.Error_status
          Raised at Lwt.Miscellaneous.poll in file "src/core/lwt.ml", line 3095, characters 20-29
          Called from Lwt_main.run.run_loop in file "src/unix/lwt_main.ml", line 31, characters 10-20
          Called from Lwt_main.run in file "src/unix/lwt_main.ml", line 60, characters 2-13
          Re-raised at Lwt_main.run in file "src/unix/lwt_main.ml", line 124, characters 4-13
          Called from Cmdliner_term.app.(fun) in file "cmdliner_term.ml", line 25, characters 19-24
          Called from Cmdliner.Term.ret.(fun) in file "cmdliner.ml", line 26, characters 27-34
          Called from Cmdliner.Term.run in file "cmdliner.ml", line 117, characters 32-39
```

Verify if your nodes are running correctly _(you should see blocks appearing in the terminal running `./sandbox.sh start`)_

⚠️ `deku-cli` can't resolve the `~` character to your home, you can use `$HOME` instead.

## Alice withdraws

**We supposed that Alice has the same address on tezos and Deku.**

```shell script
$ deku-cli withdraw "./data/0" alice.wallet "ALICE_PUBLIC_ADDRESS" 10 "(Pair \"CONTRACT_ADDRESS\" 0x)"

operation.hash: 66af83d433d69b867a00f5c9b27af92aa3ce3564e651b4b53ff17bf8a9b92f4f
```

To retrieve the tickets on tezos you have to generate of proof of your withdrawal _(using the previous `operation.hash`)_:

```shell script
$ deku-cli withdraw-proof data/0 "66af83d433d69b867a00f5c9b27af92aa3ce3564e651b4b53ff17bf8a9b92f4f" "CONTRACT_ADDRESS%withdraw"

(Pair (Pair "CONTRACT_ADDRESS%withdraw"
            (Pair (Pair 10 0x) 0 "ALICE_PUBLIC_ADDRESS")
            "CONTRACT_ADDRESS")
      0x2f79bd7d82e8df3edeb5c23d8655366f3c00dc0afa13c03b9556b59f414b5819
      {  })
```

where 
- `CONTRACT_ADDRESS%withdraw` is the address of the dummy ticket contract _(`CONTRACT_ADDRESS`)_ and its entrypoint _(`withdraw`)_ separated by `%`
- `66af83d433d69b867a00f5c9b27af92aa3ce3564e651b4b53ff17bf8a9b92f4f` is the `operation.hash` of the withdrawal operation

Save the output, as it will be used as the argument of the entrypoint of withdraw of conscensus.

### Import Alice account to Tezos

Alice must own an account on Tezos, so let's import it:

```shell script
$ tezos-client import secret key alice "unencrypted:edsk4Ye1jZ5B8t4XX1Uuxma1qB1MfkBeaasZ2NeuDvZYKCKsDHWrMa" --force
```

where `edsk4Ye1jZ5B8t4XX1Uuxma1qB1MfkBeaasZ2NeuDvZYKCKsDHWrMa` is the private key

Alice must have ꜩ in her Tezos account to pay the fees of the withdrawal:

```shell script
$ tezos-client transfer 10 from my_custom_wallet to alice --burn-cap 0.5
```

_Give some tez to alice_

Where `my_custom_wallet` is a wallet owning ꜩ.

### Withdraw the ticket to the dummy contract

```shell script
$ tezos-client transfer 0 from alice to "KT1DNb3Hap4DwkpVmiYxH7UmcjmZANtQLEFv" --entrypoint withdraw --arg "(Pair (Pair \"CONTRACT_ADDRESS%withdraw\" (Pair (Pair 10 0x) 0 \"ALICE_PUBLIC_ADDRESS\") \"CONTRACT_ADDRESS\") 0x2f79bd7d82e8df3edeb5c23d8655366f3c00dc0afa13c03b9556b59f414b5819 {  })" --burn-cap 2
```

where `-arg` is the output of the `deku-cli withdraw-proof` command.

## Verification

```shell script
$ tezos-client get contract storage for "CONTRACT_ADDRESS"

{ Pair "CONTRACT_ADDRESS" 0x 10 }
```

where 10 is the amount of ticket you withdrawed!! 🎉
