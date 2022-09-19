open Deku_repr
open Deku_crypto

type tezos_operation_hash = BLAKE2b.t
and t = tezos_operation_hash [@@deriving eq, ord]

include BLAKE2b.With_b58 (struct
  let prefix = Prefix.operation_hash
end)

module Map = Map.Make (struct
  type nonrec t = t

  let compare = BLAKE2b.compare
end)

module Set = Set.Make (struct
  type nonrec t = t

  let compare = BLAKE2b.compare
end)

include With_yojson_of_b58 (struct
  type t = tezos_operation_hash

  let of_b58 = of_b58
  let to_b58 = to_b58
end)
