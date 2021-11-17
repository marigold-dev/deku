module AST = Ast_typed
module Append_tree = Tree.Append
open Trace
open Ligo_interpreter.Types

let extract_record ~raise ~(layout:layout) (v : value) (lst : (AST.label * AST.type_expression) list) : _ list =
  match layout with
  | L_tree -> (
    let open Append_tree in
    let tree = match Append_tree.of_list lst with
      | Empty -> raise.raise @@ Errors.generic_error Location.generated "empty record"
      | Full t -> t in
    let rec aux tv : (AST.label * (value * AST.type_expression)) list =
      match tv with
      | Leaf (s, t), v -> [s, (v, t)]
      | Node {a;b}, v when Option.is_some (Ligo_interpreter.Combinators.get_pair v) ->
          let (va, vb) = trace_option ~raise (Errors.generic_error Location.generated "Expected pair") @@
                           Ligo_interpreter.Combinators.get_pair v in
          let a' = aux (a, va) in
          let b' = aux (b, vb) in
          (a' @ b')
      | _ -> raise.raise @@ Errors.generic_error Location.generated "bad record path"
    in
    aux (tree, v)
  )
  | L_comb -> (
    let rec aux lst_record v : (AST.label * (value * AST.type_expression)) list =
      match lst_record,v with
      | [], _ -> raise.raise @@ Errors.generic_error Location.generated "empty record"
      | [(s,t)], v -> [s,(v,t)]
      | [(sa,ta);(sb,tb)], v when Option.is_some (Ligo_interpreter.Combinators.get_pair v) ->
          let (va, vb) = trace_option ~raise (Errors.generic_error Location.generated "Expected pair") @@
                           Ligo_interpreter.Combinators.get_pair v in
          let a' = aux [sa, ta] va in
          let b' = aux [sb, tb] vb in
          (a' @ b')
      | (shd,thd)::tl, v when Option.is_some (Ligo_interpreter.Combinators.get_pair v) ->
        let (va, vb) = trace_option ~raise (Errors.generic_error Location.generated "Expected pair") @@
                           Ligo_interpreter.Combinators.get_pair v in
        let tl' = aux tl vb in
        ((shd,(va,thd))::tl')
      | _ -> raise.raise @@ Errors.generic_error Location.generated "bad record path"
    in
    aux lst v
  )

let extract_constructor ~raise ~(layout:layout) (v : value) (lst : (AST.label * AST.type_expression) list) : (label * value * AST.type_expression) =
  match layout with
  | L_tree ->
    let open Append_tree in
    let tree = match Append_tree.of_list lst with
      | Empty -> raise.raise @@ Errors.generic_error Location.generated "empty variant"
      | Full t -> t in
    let rec aux tv : (label * value * AST.type_expression) =
      match tv with
      | Leaf (k, t), v -> (k, v, t)
      | Node {a}, V_Construct ("Left", v) -> aux (a, v)
      | Node {b}, V_Construct ("Right", v) -> aux (b, v)
      | _ -> raise.raise @@ Errors.generic_error Location.generated "bad constructor path"
    in
    let (s, v, t) = aux (tree, v) in
    (s, v, t)
  | L_comb -> (
    let rec aux tv : (label * value * AST.type_expression) =
      match tv with
      | [], _ -> failwith "lal"
      | ((l,t)::tl), v-> ( match v with
        | V_Construct ("Left", v) -> (l,v,t)
        | V_Construct ("Right", v) -> aux (tl,v)
        | v -> (l,v,t)
      )
    in
    aux (lst,v)
  )
