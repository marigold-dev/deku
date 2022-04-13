open Crypto

module M = struct
  type t = {
    key : int;
    hash : BLAKE2B.t;
  }
  [@@deriving yojson]

  let hash t = t.hash

  let make key = { key; hash = BLAKE2B.hash (string_of_int key) }
end

open M

open Incremental_patricia.Make (M)

let size = 10

(* Note that this function does not test the stored_value in the tree as in the test
   function *)

let add_and_find tree () =
  let tree, value = add make tree in
  let _, _stored_value = find value.key tree |> Option.get in
  tree

let tree = List.init size (fun _ -> ()) |> List.fold_left add_and_find empty

let test_add_and_find () =
  let tree = tree in
  List.init size (fun n -> find n tree)

let test_hash_tree () =
  let tree = empty in
  let tree, _value_1 = add make tree in
  let tree, _value_2 = add make tree in
  let tree, _value_3 = add make tree in
  hash tree

let test_hash_values () =
  let tree = empty in
  let tree, value_1 = add make tree in
  let tree, value_2 = add make tree in
  let _tree, value_3 = add make tree in

  BLAKE2B.both
    (BLAKE2B.both value_1.hash value_2.hash)
    (BLAKE2B.both value_3.hash (hash empty))
