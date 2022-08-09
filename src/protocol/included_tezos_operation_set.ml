type included_tezos_operation_set = Deku_tezos.Operation_hash.Set.t
type t = included_tezos_operation_set

let empty = Deku_tezos.Operation_hash.Set.empty
let add = Deku_tezos.Operation_hash.Set.add

let mem tezos_operation_hash t =
  Deku_tezos.Operation_hash.Set.mem tezos_operation_hash t
