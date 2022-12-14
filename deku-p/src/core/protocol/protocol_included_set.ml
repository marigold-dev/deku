open Deku_concepts
open Protocol_operation.Initial

(* TODO: idea here is that because of that we don't need to hold the
    full operation on the protocol only it's hash temporarily*)
type included_set = Protocol_operation.Raw.Hash.Set.t Level.Map.t
and t = included_set

let empty = Level.Map.empty

let set level t =
  match Level.Map.find_opt level t with
  | Some set -> set
  | None -> Protocol_operation.Raw.Hash.Set.empty

let add raw_operation t =
  let hash = Protocol_operation.Raw.hash raw_operation in
  let level = Protocol_operation.Raw.level raw_operation in
  let set = set level t in
  let set = Protocol_operation.Raw.Hash.Set.add hash set in
  Level.Map.add level set t

let mem raw_operation t =
  let hash = Protocol_operation.Raw.hash raw_operation in
  let level = Protocol_operation.Raw.level raw_operation in
  let set = set level t in
  Protocol_operation.Raw.Hash.Set.mem hash set

let drop ~current_level t =
  (* TODO: this is O(1) but ugly, we could just drop the initial level,
      making it many times faster, does it matter? *)
  Level.Map.filter
    (fun operation_level _set ->
      is_in_includable_window ~current_level ~operation_level)
    t

let encoding = Level.Map.encoding Protocol_operation.Raw.Hash.Set.encoding
