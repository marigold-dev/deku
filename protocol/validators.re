open Helpers;
open Crypto;

// TODO: we should avoid dead validators to avoid double timeout, A(dead) -> B(dead) -> C
[@deriving (yojson, ord)]
type validator = {address: Wallet.t};

[@deriving yojson]
type t = {
  // TODO: should current be part of protocol state?
  current: option(validator),
  validators: list(validator),
  length: int,
  hash: BLAKE2B.t,
};

let current = t => t.current;
let to_list = t => t.validators;
let length = t => t.length;

let after_current = (n, t) => {
  let.some current_producer = t.current;
  // TODO: this cannot fail but I`m lazy
  let.some current_index =
    List.find_index((==)(current_producer), t.validators);
  let relative_index = (current_index + n) mod t.length;
  // TODO: module instead of remainder using mod
  let index = relative_index < 0 ? t.length + relative_index : relative_index;
  List.nth_opt(t.validators, index);
};
let update_current = (address, t) => {
  let validator =
    t.validators |> List.find_opt(validator => validator.address == address);
  {...t, current: validator};
};

let hash_validators = validators => {
  open Tezos_interop;
  let keys = validators |> List.map(validator => validator.address);
  Consensus.hash_validators(keys);
};
let empty = {
  current: None,
  validators: [],
  length: 0,
  hash: hash_validators([]),
};

// TODO: this is only okay if the number of validators is small, because, it's clearly not fast
let add = (validator, t) => {
  let validators =
    t.validators @ [validator] |> List.in_order_uniq(compare_validator);
  // TODO: is this even a good idea?
  let new_proposer = t.current == None ? Some(validator) : t.current;
  let hash = hash_validators(validators);
  {current: new_proposer, validators, length: List.length(validators), hash};
};
let remove = (validator, t) => {
  let validators = t.validators |> List.filter((!=)(validator));
  let length = List.length(validators);
  let current =
    switch (validators, t.current, validator) {
    | ([], _, _) => None
    | (_, Some(current), removed) when current == removed =>
      after_current(1, t)
    | _ => t.current
    };
  let hash = hash_validators(validators);
  {current, validators, length, hash};
};

let hash = t => t.hash;
