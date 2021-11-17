/* The function defined in "EffectfulBindingV2" is quite verbose.
   Moreover, the function *code* does not get executed when we
   define a function, so the expensive `get_contract_opt` instruction
   gets called only when the function `target_exists` is called.

   Thus, intuitively, it may be possible that we may reduce both
   the contract size and gas consumption by removing the [@inline]
   attribute. Let's check our assumption.
*/

let some_contract = ("KT1WhG8rMaC1azBJApBHW2JJdhWuhvemw4Zf" : address);

/* Calls to the function are not inlined anymore */
let target_exists = (_: unit) => {
  let c: option(contract(int)) = Tezos.get_contract_opt(some_contract);
  switch(c){
  | Some (contract) => true
  | None => false
  }
};

type parameter = Increment | IncrementIfEmpty | IncrementIfExists;

let main = ((p, s): (parameter, int)) => {
  let nop: list(operation) = [];
  switch(p){
  | Increment => (nop, s + 1)
  | IncrementIfEmpty =>
      if (target_exists()) {
        (nop, s)
      } else {
        (nop, s + 1)
      }
  | IncrementIfExists =>
      if (target_exists()) {
        (nop, s + 1)
      } else {
        (nop, s)
      }
  }
};
