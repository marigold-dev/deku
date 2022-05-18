type action =
  | Add
  | Remove
[@@deriving yojson, ord]

type t = {
  action : action;
  address : Crypto.Key_hash.t;
}
[@@deriving yojson, ord]

module Set = Set.Make (struct
  type nonrec t = t
  let compare = compare
end)
