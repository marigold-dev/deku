let check_signature (pk, signed, msg : key * signature * bytes) : bool =
  Crypto.check pk signed msg

(*
$ tezos-client gen keys testsign

$ tezos-client show address testsign -S
Hash: tz1RffmtWjy435AXZuWwLWG6UaJ66ERmgviA
Public Key: edpktz4xg6csJnJ5vcmMb2H37sWXyBDcoAp3XrBvjRaTSQ1zmZTeRQ
Secret Key: unencrypted:edsk34mH9qhMdVWtbammJfYkUoQfwW6Rw5K6rbGW1ajppy3LPNbiJA

$ tezos-client hash data '"hello"' of type string
Raw packed data: 0x05010000000568656c6c6f
...

$ tezos-client sign bytes 0x05010000000568656c6c6f for testsign
Signature: edsigtnzKd51CDomKVMFBoU8SzFZgNqRkYUaQH4DLUg8Lsimz98DFB82uiHAkdvx29DDqHxPf1noQ8noWpKMZoxTCsfprrbs4Xo
*)

let example : bool =
  Crypto.check
    ("edpktz4xg6csJnJ5vcmMb2H37sWXyBDcoAp3XrBvjRaTSQ1zmZTeRQ" : key)
    ("edsigtnzKd51CDomKVMFBoU8SzFZgNqRkYUaQH4DLUg8Lsimz98DFB82uiHAkdvx29DDqHxPf1noQ8noWpKMZoxTCsfprrbs4Xo" : signature)
    0x05010000000568656c6c6f
