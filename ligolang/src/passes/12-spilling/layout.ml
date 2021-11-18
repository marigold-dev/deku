module AST = Ast_typed
open AST
open Errors
module Append_tree = Tree.Append
open! Mini_c
open Trace
open Function

let annotation_or_label annot label = Option.value ~default:label (Helpers.remove_empty_annotation annot)

let t_sum ~raise ~layout return compile_type m =
  let open AST.Helpers in
  let lst = kv_list_of_t_sum ~layout m in
  match layout with
  | L_tree -> (
    let node = Append_tree.of_list lst in
    let aux a b : type_expression annotated =
      let t = return @@ T_or (a, b) in
      (None, t)
    in
    let m' = Append_tree.fold_ne
      (fun (Label label, ({associated_type;michelson_annotation}: AST.row_element)) ->
          let label = String.uncapitalize_ascii label in
          let a = compile_type associated_type in
          (Some (annotation_or_label michelson_annotation label), a)
      )
      aux node in
    snd m'
  )
  | L_comb -> (
    (* Right combs *)
    let aux (Label l , (x : _ row_element_mini_c )) =
      let l = String.uncapitalize_ascii l in
      let t = compile_type x.associated_type in
      let annot_opt = Some (annotation_or_label x.michelson_annotation l) in
      (annot_opt,t)
    in
    let rec lst_fold_right = function
      | [] -> raise.raise (corner_case ~loc:__LOC__ "t_record empty")
      | [ x ] -> aux x
      | hd :: tl -> (
          let hd = aux hd in
          let tl = lst_fold_right tl in
          let t = return @@ T_or (hd, tl) in
          (None,t)
        )
    in
    snd @@ lst_fold_right lst
  )

(* abstract description of final physical record layout, with
   corresponding compiled types: *)
type record_tree_content =
  | Field of label
  | Pair of record_tree * record_tree

and record_tree = {
  content : record_tree_content ;
  type_ : Mini_c.type_expression
}

(* Based on t_record_to_pairs. Can this be used to simplify the other
   record things in this file? *)
(* ...also there must be a better way to write this, is it even
   correct, I'm not sure? *)
let record_tree ~layout compile_type m =
  let open AST.Helpers in
  let is_tuple_lmap = is_tuple_lmap m in
  let lst = kv_list_of_t_record_or_tuple ~layout m in
  match layout with
  | L_tree -> (
      let node = Append_tree.of_list lst in
      let aux (a_annot, a) (b_annot, b) =
        (None, { content = Pair (a, b) ;
                    type_ = Expression.make_t (T_tuple [(a_annot, a.type_); (b_annot, b.type_)]) })
      in
      let m' = Append_tree.fold_ne
          (fun (Label label, ({associated_type;michelson_annotation}: AST.row_element)) ->
             let a = compile_type associated_type in
             let annot = (if is_tuple_lmap then
                            None
                          else
                            Some (annotation_or_label michelson_annotation label)) in
             (annot, { content = Field (Label label) ;
                          type_ = a })
          )
          aux node in
      snd m'
    )
  (* TODO this is unused now but still called... *)
  | L_comb ->
    {content = Field (Label "BOGUS");
        type_ = { type_content = T_base TB_unit;
                  location = Location.generated }}

let t_record_to_pairs ~layout return compile_type m =
  let open AST.Helpers in
  let is_tuple_lmap = is_tuple_lmap m in
  let lst = kv_list_of_t_record_or_tuple ~layout m in
  match layout with
  | L_tree -> (
      let node = Append_tree.of_list lst in
      let aux a b : type_expression annotated =
        let t = return @@ T_tuple [a; b] in
        (None, t)
      in
      let m' = Append_tree.fold_ne
          (fun (Label label, ({associated_type;michelson_annotation}: AST.row_element)) ->
             let a = compile_type associated_type in
             ((if is_tuple_lmap then
                    None
                  else
                    Some (annotation_or_label michelson_annotation label)),
                 a)
          )
          aux node in
      snd m'
    )
  | L_comb -> (
      (* Right combs *)
      let aux (Label l , (x : _ row_element_mini_c)) =
        let t = compile_type x.associated_type in
        let annot_opt = Some (annotation_or_label x.michelson_annotation l) in
        (annot_opt,t)
      in
      let ts = List.map ~f:aux lst in
      return (T_tuple ts)
    )

let record_access_to_lr ~raise ~layout ty m_ty index =
  let open AST.Helpers in
  let lst = kv_list_of_t_record_or_tuple ~layout m_ty in
  match layout with
  | L_tree -> (
      let node_tv = Append_tree.of_list lst in
      let path =
        trace_option ~raise (corner_case ~loc:__LOC__ "record access leaf") @@
        Append_tree.exists_path_to index node_tv
      in
      let lr_path = List.map ~f:(fun b -> if b then `Right else `Left) path in
        let aux = fun (ty , acc) cur ->
          let (a , b) =
            trace_option ~raise (corner_case ~loc:__LOC__ "record access pair") @@
            Mini_c.get_t_pair ty
          in
          match cur with
          | `Left  -> (a , (a , `Left)  :: acc)
          | `Right -> (b , (b , `Right) :: acc )
        in
        (List.rev <@ snd) @@ List.fold ~f:aux ~init:(ty , []) lr_path
    )
  | L_comb -> (
      let rec aux n ty last =
        match n , last with
        | 0 , true -> []
        | 0 , false -> (
            let (a , _) =
              trace_option ~raise (corner_case ~loc:__LOC__ "record access pair") @@
              Mini_c.get_t_pair ty
            in
            [(a , `Left)]
          )
        | n , last -> (
            let (_ , b) =
              trace_option ~raise (corner_case ~loc:__LOC__ "record access pair") @@
              Mini_c.get_t_pair ty
            in
            let prec = aux (n - 1) b last in
            ((b , `Right)::prec)
          )
      in
      let index =
        fst @@
        Trace.trace_option ~raise (corner_case ~loc:__LOC__ "constructor access") @@
        (List.findi ~f:(fun _ (label , _) -> label = index) lst)
      in
      let last = (index + 1 = List.length lst) in
      aux index ty last
    )

let record_to_pairs ~raise compile_expression return record_t record : Mini_c.expression =
  let open AST.Helpers in
  let lst = kv_list_of_record_or_tuple ~layout:record_t.layout record_t.content record in
  match record_t.layout with
  | L_tree -> (
    let node = Append_tree.of_list lst in
    let aux a b : expression =
      return @@ ec_pair a b
    in
    trace ~raise (fun _ -> corner_case ~loc:__LOC__ "record build") @@
    (fun ~raise:_-> Append_tree.fold_ne (compile_expression) aux node)
  )
  | L_comb -> (
      let exprs = List.map ~f:compile_expression lst in
      return (E_tuple exprs)
    )

let constructor_to_lr ~raise ~(layout) ty m_ty index =
  let open AST.Helpers in
  let lst = kv_list_of_t_sum ~layout m_ty in
  match layout with
  | L_tree -> (
      let node_tv = Append_tree.of_list lst in
      let path =
        trace_option ~raise (corner_case ~loc:__LOC__ "constructor leaf") @@
        Append_tree.exists_path_to index node_tv
      in
      let lr_path = List.map ~f:(fun b -> if b then `Right else `Left) path in
      let (_ , lst) =
        let aux = fun (ty , acc) cur ->
          let (a , b) =
            trace_option ~raise (corner_case ~loc:__LOC__ "constructor union") @@
            Mini_c.get_t_or ty
          in
          match cur with
          | `Left  -> (a , (ty , `Left)  :: acc)
          | `Right -> (b , (ty , `Right) :: acc )
        in
        List.fold ~f:aux ~init:(ty , []) lr_path
      in
      lst
    )
  | L_comb -> (
    let index =
      fst @@
      Trace.trace_option ~raise (corner_case ~loc:__LOC__ "constructor access") @@
      (List.findi ~f:(fun _ (label , _) -> label = index) lst)
    in
    let last = (index + 1 = List.length lst) in
    let rec aux n ty =
      match n , last with
      | 0 , true -> []
      | 0 , false -> [(ty , `Left)]
      | n , _ -> (
          let (_ , b) =
            trace_option ~raise (corner_case ~loc:__LOC__ "constructor union") @@
            Mini_c.get_t_or ty
          in
          let prec = aux (n - 1) b in
          ((ty , `Right)::prec)
        )
    in
    List.rev @@ aux index ty
    )

type variant_tree = [
  | `Leaf of label
  | `Node of variant_pair * variant_pair
]

and variant_pair = variant_tree * Mini_c.type_expression

let match_variant_to_tree ~raise ~layout ~compile_type content : variant_pair =
  match layout with
  | L_tree -> (
      let kt_tree =
        let kt_list = List.map ~f:(fun (k,({associated_type;_}:AST.row_element)) -> (k,associated_type)) (LMap.to_kv_list content) in
        Append_tree.of_list kt_list
      in
      let ne_tree = match kt_tree with
        | Empty -> raise.raise (corner_case ~loc:__LOC__ "match empty variant")
        | Full x -> x in
      let vp =
        let rec aux t : variant_pair =
          match (t : _ Append_tree.t') with
          | Leaf (name , tv) ->
            let tv' = compile_type tv in
            (`Leaf name , tv')
          | Node {a ; b} ->
            let a' = aux a in
            let b' = aux b in
            let tv' = t_union (None, snd a') (None, snd b') in
            (`Node (a' , b') , tv')
        in
        aux ne_tree
      in
      vp
    )
  | L_comb -> (
      let rec aux : _ -> variant_pair = function
        | [] -> raise.raise (corner_case ~loc:__LOC__ "variant build")
        | [(k , ty)] -> (
            let t = compile_type ty in
            (`Leaf k , t)
          )
        | (khd , thd)::tl -> (
            let thd' = compile_type thd in
            let (tl',ttl) = aux tl in
            let tv' = t_union (None, thd') (None, ttl) in
            let left = `Leaf khd , thd' in
            (`Node (left , (tl' , ttl)) , tv')
          ) in
      let lst = List.map ~f:(fun (k,({associated_type;_} : _ row_element_mini_c)) -> (k,associated_type)) @@ Ast_typed.Helpers.kv_list_of_t_sum ~layout content in
      let vp = aux lst in
      vp
    )

let extract_record ~raise ~(layout:layout) (v : value) (lst : (AST.label * AST.type_expression) list) : _ list =
  match layout with
  | L_tree -> (
    let open Append_tree in
    let tree = match Append_tree.of_list lst with
      | Empty -> raise.raise @@ corner_case ~loc:__LOC__ "empty record"
      | Full t -> t in
    let rec aux tv : (AST.label * (value * AST.type_expression)) list =
      match tv with
      | Leaf (s, t), v -> [s, (v, t)]
      | Node {a;b}, D_pair (va, vb) ->
          let a' = aux (a, va) in
          let b' = aux (b, vb) in
          (a' @ b')
      | _ -> raise.raise @@ corner_case ~loc:__LOC__ "bad record path"
    in
    aux (tree, v)
  )
  | L_comb -> (
    let rec aux lst_record v : (AST.label * (value * AST.type_expression)) list =
      match lst_record,v with
      | [], _ -> raise.raise @@ corner_case ~loc:__LOC__ "empty record"
      | [(s,t)], v -> [s,(v,t)]
      | [(sa,ta);(sb,tb)], D_pair (va,vb) ->
          let a' = aux [sa, ta] va in
          let b' = aux [sb, tb] vb in
          (a' @ b')
      | (shd,thd)::tl, D_pair (va,vb) ->
        let tl' = aux tl vb in
        ((shd,(va,thd))::tl')
      | _ -> raise.raise @@ corner_case ~loc:__LOC__ "bad record path"
    in
    aux lst v
  )

let extract_constructor ~raise ~(layout:layout) (v : value) (lst : (AST.label * AST.type_expression) list) : (label * value * AST.type_expression) =
  match layout with
  | L_tree ->
    let open Append_tree in
    let tree = match Append_tree.of_list lst with
      | Empty -> raise.raise @@ corner_case ~loc:__LOC__ "empty variant"
      | Full t -> t in
    let rec aux tv : (label * value * AST.type_expression) =
      match tv with
      | Leaf (k, t), v -> (k, v, t)
      | Node {a}, D_left v -> aux (a, v)
      | Node {b}, D_right v -> aux (b, v)
      | _ -> raise.raise @@ corner_case ~loc:__LOC__ "bad constructor path"
    in
    let (s, v, t) = aux (tree, v) in
    (s, v, t)
  | L_comb -> (
    let rec aux tv : (label * value * AST.type_expression) =
      match tv with
      | [], _ -> failwith "lal"
      | ((l,t)::tl), v-> ( match v with
        | D_left v -> (l,v,t)
        | D_right v -> aux (tl,v)
        | v -> (l,v,t)
      )
    in
    aux (lst,v)
  )
