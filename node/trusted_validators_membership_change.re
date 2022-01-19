open Core;
open Helpers;

[@deriving (yojson, ord)]
type action =
  | Add
  | Remove;

[@deriving (yojson, ord)]
type t = {
  action,
  address: Address.t,
};

module Set =
  Set.Make_with_yojson({
    type nonrec t = t;
    let of_yojson = of_yojson;
    let to_yojson = to_yojson;
    let compare = compare;
  });
