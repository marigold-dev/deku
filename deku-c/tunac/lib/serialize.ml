open Tezos_micheline
open Micheline
(* open Michelson_v1_primitives *)

let int32_to_bytes n =
  let buffer = Bytes.create 4 in
  Bytes.set_int32_le buffer 0 n;
  buffer

let rec compile_value node =
  match node with
  | Int (_, n) ->
    int32_to_bytes (Z.to_int32 n)
  
  | Prim (_, "Elt", args, _)
  | Prim (_, "Pair", args, _) ->
    Bytes.concat Bytes.empty (List.map compile_value args)

  | Prim (_, "Some", [ arg ], _)
  | Prim (_, "Left", [ arg ], _) ->
    Bytes.(cat (int32_to_bytes 1l) (compile_value arg))

  | Prim (_, "Right", [ arg ], _) ->
    Bytes.(cat (int32_to_bytes 0l) (compile_value arg))

  | Prim (_, "None", _, _)
  | Prim (_, "False", [], [])
  | Prim (_, "Unit", [], _) ->
    int32_to_bytes 0l

  | Prim (_, "True", [], []) ->
    int32_to_bytes 0xffffffffl

  | Seq (_, lst) ->
    let len = Int32.of_int (List.length lst) in
    Bytes.(cat
      (int32_to_bytes len)
      (concat empty (List.(map compile_value (rev lst)))))

  | String (_, s) ->
    let len = Int32.of_int (String.length s) in
    Bytes.(cat (int32_to_bytes len) (of_string s))

  | Bytes (_, s) ->
    let len = Int32.of_int (Bytes.length s) in
    Bytes.(cat (int32_to_bytes len) s)

  | _ -> assert false
