open Types

type t = environment
type element = environment_element

let make_element : type_expression -> environment_element_definition -> element =
  fun type_value definition -> {type_value ; definition}

let make_element_binder = fun t -> make_element t ED_binder

let make_element_declaration = fun (expr : expression) (ty_expr : type_expression) ->
  let free_variables = Misc.Free_variables.(expression empty expr) in
  make_element ty_expr (ED_declaration {expression=expr ; free_variables})

let empty : t = { expression_environment = [] ; type_environment = [] ; module_environment = [] }

let get_expr_environment : t -> expression_environment = fun { expression_environment ; type_environment=_ ; module_environment=_ } -> expression_environment
(* TODO: generate *)
let get_type_environment : t -> type_environment = fun { expression_environment=_ ; type_environment ; module_environment=_} -> type_environment
(* TODO: generate *)
let get_module_environment : t -> module_environment = fun { expression_environment=_ ; type_environment=_ ; module_environment} -> module_environment

let map_expr_environment : _ -> t -> t = fun f { expression_environment ; type_environment ; module_environment } -> { expression_environment = f expression_environment ; type_environment ; module_environment }
let map_type_environment : _ -> t -> t = fun f { expression_environment ; type_environment ; module_environment } -> { expression_environment ; type_environment = f type_environment ; module_environment }
let map_module_environment : _ -> t -> t = fun f { expression_environment ; type_environment ; module_environment } -> { expression_environment ; type_environment ; module_environment = f module_environment }

let add_expr   : expression_variable -> element -> t -> t = fun expr_var env_elt -> map_expr_environment (fun x -> {expr_var ; env_elt} :: x)
let add_type   : type_variable -> type_expression -> t -> t = fun type_variable type_ -> map_type_environment (fun x -> { type_variable ; type_ } :: x)
let add_module : module_variable -> environment -> t -> t = fun module_variable module_ -> map_module_environment (fun x -> { module_variable ; module_ } :: x)
(* TODO: generate : these are now messy, clean them up. *)

let of_list_type : (type_variable * type_expression) list -> t = fun tvlist -> List.fold_left ~f:(fun acc (t,v) -> add_type t v acc) ~init:empty tvlist

let get_opt : expression_variable -> t -> element option = fun k x ->
  Option.bind ~f: (fun {expr_var=_ ; env_elt} -> Some env_elt) @@
    List.find ~f:(fun {expr_var ; env_elt=_} -> Var.equal expr_var.wrap_content k.wrap_content) (get_expr_environment x)
let get_type_opt : type_variable -> t -> type_expression option = fun k x ->
  Option.bind ~f: (fun {type_variable=_ ; type_} -> Some type_) @@
    List.find ~f:(fun {type_variable ; type_=_} -> Var.equal type_variable k) (get_type_environment x)
let get_module_opt : module_variable -> t -> environment option = fun k x ->
  Option.bind ~f: (fun {module_variable=_ ; module_} -> Some module_) @@
    List.find ~f:(fun {module_variable; module_=_} -> String.equal module_variable k) (get_module_environment x)

let add_ez_binder : expression_variable -> type_expression -> t -> t = fun k v e ->
  add_expr k (make_element_binder v) e

let add_ez_declaration : expression_variable -> expression -> type_expression -> t -> t = fun k e t env ->
  add_expr k (make_element_declaration e t) env

let get_constructor : label -> t -> (type_expression * type_expression) option = fun k x -> (* Left is the constructor, right is the sum type *)
  let rec rec_aux e =
    let aux = fun {type_variable=_ ; type_} ->
      match type_.type_content with
      | T_sum m ->
        (match LMap.find_opt k m.fields with
            Some {associated_type ; _} -> Some (associated_type , type_)
          | None -> None)
      | _ -> None
    in
    match List.find_map ~f:aux (get_type_environment e) with
      Some _ as s -> s
    | None ->
      let modules = get_module_environment e in
      List.fold_left ~f:(fun res {module_variable=_;module_} ->
        match res with Some _ as s -> s | None -> rec_aux module_
      ) ~init:None modules
  in rec_aux x

let get_record : _ label_map -> t -> (rows) option = fun lmap e ->
  let rec rec_aux e =
    let aux = fun {type_variable=_ ; type_} ->
      match type_.type_content with
      | T_record m -> Option.(
        let lst_kv  = LMap.to_kv_list_rev lmap in
        let lst_kv' = LMap.to_kv_list_rev m.fields in
        let m = map ~f:(fun () -> m) @@ Misc.assert_list_eq
          ( fun (ka,va) (kb,vb) ->
            let Label ka = ka in
            let Label kb = kb in
            let* () = Misc.assert_eq ka kb in
            Misc.assert_type_expression_eq (va.associated_type, vb.associated_type)
          ) lst_kv lst_kv' in
        m
      )
      | _ -> None
    in
    match List.find_map ~f:aux (get_type_environment e) with
      Some _ as s -> s
    | None ->
      let modules = get_module_environment e in
      List.fold_left ~f:(fun res {module_variable=_;module_} ->
        match res with Some _ as s -> s | None -> rec_aux module_
      ) ~init:None modules
  in rec_aux e


let get_sum : _ label_map -> t -> rows option = fun lmap e ->
  let rec rec_aux e =
    let aux = fun {type_variable=_ ; type_} ->
      match type_.type_content with
      | T_sum m -> Option.(
        let lst_kv  = LMap.to_kv_list_rev lmap in
        let lst_kv' = LMap.to_kv_list_rev m.fields in
        map ~f:(fun () -> m) @@ Misc.assert_list_eq (
          fun (ka,va) (kb,vb) ->
            let Label ka = ka in
            let Label kb = kb in
            let* () = Misc.assert_eq ka kb in
            Misc.assert_type_expression_eq (va.associated_type, vb.associated_type)
        ) lst_kv lst_kv'
      )
      | _ -> None
    in
    match List.find_map ~f:aux (get_type_environment e) with
      Some _ as s -> s
    | None ->
      let modules = get_module_environment e in
      List.fold_left ~f:(fun res {module_variable=_;module_} ->
        match res with Some _ as s -> s | None -> rec_aux module_
      ) ~init:None modules
  in rec_aux e

module PP = struct
  open Format
  include PP
  open PP_helpers

  let list_sep_scope x = list_sep x (const " | ")

  let rec environment_element = fun ppf {expr_var ; env_elt} ->
    fprintf ppf "%a -> %a \n" PP.expression_variable expr_var PP.type_expression env_elt.type_value

  and type_environment_element = fun ppf {type_variable ; type_} ->
    fprintf ppf "%a -> %a" PP.type_variable type_variable PP.type_expression type_

  and expr_environment : _ -> expression_environment -> unit = fun ppf lst ->
    fprintf ppf "Env:[%a]" (list_sep environment_element (tag "@,")) lst

  and type_environment = fun ppf lst ->
    fprintf ppf "Type env:[%a]" (list_sep type_environment_element (tag "@,")) lst

  and environment : _ -> environment -> unit = fun ppf e ->
    fprintf ppf "- %a\t%a"
      expr_environment (get_expr_environment e)
      type_environment (get_type_environment e)

end
