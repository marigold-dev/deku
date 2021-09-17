open Protocol;

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
  Set.Make({
    type nonrec t = t;
    let compare = compare;
  });
