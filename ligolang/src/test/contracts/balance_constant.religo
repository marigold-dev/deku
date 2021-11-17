/**

This test makes sure that the balance is accessible in ReasonLIGO.
It's there to detect a regression of: https://gitlab.com/ligolang/ligo/issues/61

Which results in this error when you attempt to compile this contract:

generated. unrecognized constant: {"constant":"BALANCE","location":"generated"}


*/

type storage = tez;

let main2 = (_ : unit, _: storage) =>
  ([]: list (operation), Tezos.balance);

let main = (x : (unit, storage)) => main2 (x[0], x[1]);
