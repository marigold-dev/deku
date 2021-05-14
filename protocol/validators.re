open Helpers;

// TODO: we should avoid dead validators to avoid double timeout, A(dead) -> B(dead) -> C
[@deriving (yojson, ord)]
type validator = {
  address: Address.t,
  uri: Uri.t,
};

[@deriving yojson]
type t = {
  // TODO: should current be part of protocol state?
  current: option(validator),
  validators: list(validator),
};

let current = t => t.current;
let validators = t => t.validators;
let next_proposer = t =>
  switch (t.current) {
  | Some(current_proposer) =>
    let index =
      List.find_index((==)(current_proposer), t.validators) |> Option.get;
    let length = List.length(t.validators);
    let next_index = index + 1 >= length ? 0 : index + 1;
    List.nth_opt(t.validators, next_index);
  | None => List.nth_opt(t.validators, 0)
  };
let next = t => {...t, current: next_proposer(t)};

let update_current = (address, t) => {
  let validator =
    t.validators |> List.find_opt(validator => validator.address == address);
  {...t, current: validator};
};
let empty = {current: None, validators: []};
// TODO: this is only okay if the number of validators is small, because, it's clearly not fast
let add = (validator, t) => {
  let validators = t.validators @ [validator] |> List.dumb_uniq((==));
  let new_proposer = t.current == None ? Some(validator) : t.current;
  {current: new_proposer, validators};
};
let remove = (validator, t) => {
  let new_proposer =
    t.current == Some(validator) ? next_proposer(t) : t.current;
  let new_proposer = new_proposer == Some(validator) ? None : new_proposer;
  let new_validators = t.validators |> List.filter((!=)(validator));

  {current: new_proposer, validators: new_validators};
};
