# How to do a deposit and withdraw on deku

Here is the scenario of this end to end test:

- start deku
- originate a ticket contract
- deposit ticket on deku
- transfer to alice
- withdraw as alice

## Start deku cluster

```bash
docker compose up -d # To start flextesa (sandbox tezos node)
./sandbox.sh setup
./sandbox.sh start # (blocks should appear)
```

## Deploy a contract that holds tickets

Creates a contract which can deposit or withraw tickets from the conscensus contract

```
./sandbox.sh deploy-dummy-ticket

> =========== Running in local mode ===========
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

> New contract KT1WyREzcYATwbcbGPnLvDvUAHQcY7SJLm82 originated.

### Possible error (solved by retrying):

```bash
=========== Running in local mode ===========
Node is bootstrapped.
Estimated gas: 1428.723 units (will add 100 for safety)
Estimated storage: 547 bytes added (will add 20 for safety)
Error while applying operation ooZ7rLm9EF1paqBGb9s2Ddv2GDR2dqkV9QbLMXBpJ4ary5UtUxS:
Error:
  Counter 4 already used for contract tz1VSUr8wwNhLAzempoch5d6hLRiTh8Cjcjb (expected 5)
Fatal error:
  origination simulation failed
```

## Deposit tickets from your contract to deku

Creates `n` tickets (10000000 defined in the contract) and deposit them in the conscensus contract so that there are available in deku for the address `tz1RPNjHPWuM8ryS5LDttkHdM321t85dSqaf` (deku address hard coded in the `sandbox.sh` script)

```bash
./sandbox.sh deposit-dummy-ticket

>
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
      To: KT1WyREzcYATwbcbGPnLvDvUAHQcY7SJLm82
      Entrypoint: deposit
      Parameter: (Pair (Pair 0x "KT1DNb3Hap4DwkpVmiYxH7UmcjmZANtQLEFv")
                       "tz1RPNjHPWuM8ryS5LDttkHdM321t85dSqaf")
      This transaction was successfully applied
      Updated storage: {}
      Storage size: 290 bytes
      Consumed gas: 2808.124
    Internal operations:
      Transaction:
        Amount: ꜩ0
        From: KT1WyREzcYATwbcbGPnLvDvUAHQcY7SJLm82
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

## Wallet creation

Alice need an address on deku

```bash
deku-cli create-wallet
```

and save the field `address`

and rename it to alice.wallet

Creates a wallet for the ticket holder named wallet.json:

```json
{
  "address": "tz1RPNjHPWuM8ryS5LDttkHdM321t85dSqaf", // deku address from sandbox script
  "priv_key": "edsk36FhrZwFVKpkdmouNmcwkAJ9XgSnE5TFHA7MqnmZ93iczDhQLK" // deku secret from sandbox script
}
```

## Transfer ticket to alice

Transfer 10 tickets from `wallet.json` to alice

```bash
deku-cli create-transaction data/0 wallet.json "tz1N781N76jRmVZgzZFgoNfyi77rw6f7WoDa" 10 "(Pair \"KT1WyREzcYATwbcbGPnLvDvUAHQcY7SJLm82\" 0x)"
> operation.hash: a4ebc04591f2806045ede1dbc4be9b4a4eff503be69cc970132a2ec1ec7f05db
```

where `tz1N781N76jRmVZgzZFgoNfyi77rw6f7WoDa` is the public address of alice
and `KT1WyREzcYATwbcbGPnLvDvUAHQcY7SJLm82` the dummy ticket contract address

So now alice has 10 tickets

### Possible error:

If you have this following error:

```
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

Verify if your nodes are running correctly (you should see blocks appearing in the terminal where you did `./sandbox.sh start`)

`deku-cli` can't resolve the `~` character to your home, so if you want it you can use `$HOME`

## Alice withdraw

We supposed that alice has the same address on tezos and deku

```bash
deku-cli withdraw "./data/0" alice.wallet "tz1N781N76jRmVZgzZFgoNfyi77rw6f7WoDa" 10 "(Pair \"KT1WyREzcYATwbcbGPnLvDvUAHQcY7SJLm82\" 0x)"
> operation.hash: 66af83d433d69b867a00f5c9b27af92aa3ce3564e651b4b53ff17bf8a9b92f4f
```

To retrieve the tickets on tezos you have to generate of proof of your withdrawal

```
deku-cli withdraw-proof data/0 "66af83d433d69b867a00f5c9b27af92aa3ce3564e651b4b53ff17bf8a9b92f4f" "KT1WyREzcYATwbcbGPnLvDvUAHQcY7SJLm82%withdraw"
>
(Pair (Pair "KT1WyREzcYATwbcbGPnLvDvUAHQcY7SJLm82%withdraw"
            (Pair (Pair 10 0x) 0 "tz1N781N76jRmVZgzZFgoNfyi77rw6f7WoDa")
            "KT1WyREzcYATwbcbGPnLvDvUAHQcY7SJLm82")
      0x2f79bd7d82e8df3edeb5c23d8655366f3c00dc0afa13c03b9556b59f414b5819
      {  })
```

where `KT1WyREzcYATwbcbGPnLvDvUAHQcY7SJLm82%withdraw` is the address of the dummy ticket contract and its entrypoint
where `66af83d433d69b867a00f5c9b27af92aa3ce3564e651b4b53ff17bf8a9b92f4f`is the hash of the withdrawal operation
where the output is the argument of the entrypoint of withdraw of conscensus

### Alice account has to exists in tezos, so we import it

```bash
tezos-client import secret key alice "unencrypted:edsk4Ye1jZ5B8t4XX1Uuxma1qB1MfkBeaasZ2NeuDvZYKCKsDHWrMa" --force
```

where `edsk4Ye1jZ5B8t4XX1Uuxma1qB1MfkBeaasZ2NeuDvZYKCKsDHWrMa` is the private key

Alice must have tez in its tezos account to pay the fees of the withdraw

```
tezos-client transfer 10 from my_custom_wallet to alice --burn-cap 0.5 # Just to give some tez to alice
```

### Withdraw the ticket to the dummy contract

```bash
tezos-client transfer 0 from alice to "KT1DNb3Hap4DwkpVmiYxH7UmcjmZANtQLEFv" --entrypoint withdraw --arg "(Pair (Pair \"KT1WyREzcYATwbcbGPnLvDvUAHQcY7SJLm82%withdraw\" (Pair (Pair 10 0x) 0 \"tz1N781N76jRmVZgzZFgoNfyi77rw6f7WoDa\") \"KT1WyREzcYATwbcbGPnLvDvUAHQcY7SJLm82\") 0x2f79bd7d82e8df3edeb5c23d8655366f3c00dc0afa13c03b9556b59f414b5819 {  })" --burn-cap 2
```

where the arg is the output of the withdraw-proof command.

## Verification

```bash
tezos-client get contract storage for "KT1WyREzcYATwbcbGPnLvDvUAHQcY7SJLm82"
> { Pair "KT1WyREzcYATwbcbGPnLvDvUAHQcY7SJLm82" 0x 10 }
```

where 10 is the amount of ticket you withdrawed
