open Errors
open Ast_imperative
open Trace

let peephole_expression ~raise : expression -> expression = fun e ->
  let return expression_content = { e with expression_content } in
  match e.expression_content with
  | E_literal (Literal_key_hash s) as l -> (
    let open Tezos_crypto in
    match Signature.Public_key_hash.of_b58check_opt s with
    | None -> raise.raise (bad_format e)
    | Some _ -> return l
    )
  | E_literal (Literal_address _) as l -> (
    return l
    )
  | E_literal (Literal_signature s) as l -> (
    let open Tezos_crypto in
    match Signature.of_b58check_opt s with
    | None -> raise.raise (bad_format e)
    | Some _ -> return l
    )
  | E_literal (Literal_key s) as l -> (
    let open Tezos_crypto in
    match Signature.Public_key.of_b58check_opt s with
    | None -> raise.raise (bad_format e)
    | Some _ ->
      return l
    )
  | E_constant {cons_name=cst; arguments=lst} as e_const ->
     let cst = const_name cst in
     begin match cst with
     | C_BIG_MAP_LITERAL -> (
       let elt =
         trace_option ~raise (bad_single_arity cst e) @@
           List.to_singleton lst
       in
       let lst =
         trace_option ~raise (bad_map_param_type cst e) @@
           get_e_list elt.expression_content
       in
       let aux = fun (e : expression) ->
         trace_option ~raise (bad_map_param_type cst e) @@
           Option.(let* t = get_e_tuple e.expression_content in
                   List.to_pair t)
       in
       let pairs = List.map ~f:aux lst in
       return @@ E_big_map pairs
     )
     | C_MAP_LITERAL -> (
       let elt =
         trace_option ~raise (bad_single_arity cst e) @@
           List.to_singleton lst
       in
       let lst =
         trace_option ~raise (bad_map_param_type cst e) @@
           get_e_list elt.expression_content
       in
       let aux = fun (e : expression) ->
         trace_option ~raise (bad_map_param_type cst e) @@
           Option.(let* t = get_e_tuple e.expression_content in
                   List.to_pair t)
       in
       let pairs = List.map ~f:aux lst in
       return @@ E_map pairs
     )
     | C_BIG_MAP_EMPTY -> (
       let () =
         Assert.assert_list_empty ~raise (bad_empty_arity cst e) lst
       in
       return @@ E_big_map []
     )
     | C_MAP_EMPTY -> (
       let () =
         Assert.assert_list_empty ~raise (bad_empty_arity cst e) lst
       in
       return @@ E_map []
     )

     | C_SET_LITERAL -> (
       let elt =
         trace_option ~raise (bad_single_arity cst e) @@
           List.to_singleton lst
       in
       let lst =
         trace_option ~raise (bad_set_param_type cst e) @@
           get_e_list elt.expression_content
       in
       return @@ E_set lst
     )
     | C_SET_EMPTY -> (
       let () =
         Assert.assert_list_empty ~raise (bad_empty_arity cst e) lst
       in
       return @@ E_set []
     )
     | _ -> return e_const end
  | e -> return e
