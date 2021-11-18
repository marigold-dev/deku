(*
This test makes sure that the balance is accessible in CameLIGO.

It is there to detect a regression of:
https://gitlab.com/ligolang/ligo/issues/61

which results in this error when you attempt to compile this contract:

generated. unrecognized constant: {"constant":"BALANCE","location":"generated"}

*)

type parameter = unit
type storage = tez
type return = operation list * storage

let main (_, _ : parameter * storage) : return =
  ([] : operation list), Tezos.balance
