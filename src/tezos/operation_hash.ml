open Deku_repr
open Deku_crypto

type t = BLAKE2b.t [@@deriving eq, ord, yojson]

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