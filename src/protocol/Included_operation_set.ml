open Deku_concepts
open Operation

(* TODO: idea here is that because of that we don't need to hold the
    full operation on the protocol only it's hash temporarily*)
type included_operation_set = Level.t Operation_hash.Map.t
type t = included_operation_set

let empty = Operation_hash.Map.empty

let add operation t =
  let (Operation { hash; level; _ }) = operation in
  Operation_hash.Map.add hash level t

let mem operation t =
  let (Operation { hash; _ }) = operation in
  Operation_hash.Map.mem hash t

let drop ~current_level t =
  Operation_hash.Map.filter
    (fun _hash operation_level ->
      Operation.is_in_includable_window ~current_level ~operation_level)
    t
