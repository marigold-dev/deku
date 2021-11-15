open Types

(* TODO: does that need to be cleaned-up ? *)
module Free_variables = struct

  type bindings = expression_variable list
  let var_equal = Location.equal_content ~equal:Var.equal
  let mem : bindings -> expression_variable -> bool = List.mem ~equal:var_equal
  let singleton : expression_variable -> bindings = fun s -> [ s ]
  let union : bindings -> bindings -> bindings = (@)
  let unions : bindings list -> bindings = List.concat
  let empty : bindings = []

  let rec expression_content : bindings -> expression_content -> bindings = fun b ec ->
    let self = expression b in
    match ec with
    | E_lambda l -> lambda b l
    | E_literal _ -> empty
    | E_constant {arguments;_} -> unions @@ List.map ~f:self arguments
    | E_variable name -> (
        match mem b name with
        | true -> empty
        | false -> singleton name
      )
    | E_application {lamb;args} -> unions @@ List.map ~f:self [ lamb ; args ]
    | E_constructor {element;_} -> self element
    | E_record m -> unions @@ List.map ~f:self @@ LMap.to_list m
    | E_record_accessor {record;_} -> self record
    | E_record_update {record; update;_} -> union (self record) @@ self update
    | E_matching {matchee; cases;_} -> union (self matchee) (matching_expression b cases)
    | E_let_in { let_binder; rhs; let_result; _} ->
      let b' = union (singleton let_binder) b in
      union
        (expression b' let_result)
        (self rhs)
    | E_type_in { type_binder=_; rhs=_; let_result; _} -> self let_result
    | E_mod_in { module_binder=_; rhs=_; let_result} -> self let_result
    | E_mod_alias { alias=_; binders=_; result} -> self result
    | E_raw_code _ -> empty
    | E_type_inst {type_=_;forall} -> self forall
    | E_recursive {fun_name;lambda;_} ->
      let b' = union (singleton fun_name) b in
      expression_content b' @@ E_lambda lambda
    | E_module_accessor {element;_} -> self element

  and lambda : bindings -> lambda -> bindings = fun b l ->
    let b' = union (singleton l.binder) b in
    expression b' l.result

  and expression : bindings -> expression -> bindings = fun b e ->
    expression_content b e.expression_content

    and matching_variant_case : (bindings -> expression -> bindings) -> bindings -> matching_content_case -> bindings  = fun f b { constructor=_ ; pattern ; body } ->
      f (union (singleton pattern) b) body
  
    and matching : (bindings -> expression -> bindings) -> bindings -> matching_expr -> bindings = fun f b m ->
      match m with
      | Match_variant { cases ; tv=_ } -> unions @@ List.map ~f:(matching_variant_case f b) cases
      | Match_record {fields; body; tv = _} ->
        f (union (List.map ~f:fst (LMap.to_list fields)) b) body

    and matching_expression = fun x -> matching expression x

end


let assert_eq = fun a b -> if (a = b) then Some () else None
let assert_same_size = fun a b -> if (List.length a = List.length b) then Some () else None
let rec assert_list_eq f = fun a b -> match (a,b) with
  | [], [] -> Some ()
  | [], _  -> None
  | _ , [] -> None
  | hda::tla, hdb::tlb -> Option.(
    let* () = f hda hdb in
    assert_list_eq f tla tlb
  )

let layout_eq a b = match (a,b) with
  | L_comb, L_comb
  | L_tree, L_tree -> true
  | _ -> false

let constant_compare ia ib =
  let open Stage_common.Constant in
  let ia' = Ligo_string.extract ia in
  let ib' = Ligo_string.extract ib in
  match ia',ib' with
  | a,b when (String.equal a map_name || String.equal a map_or_big_map_name) && (String.equal b map_name || String.equal b map_or_big_map_name) -> 0
  | a,b when (String.equal a big_map_name || String.equal a map_or_big_map_name) && (String.equal b big_map_name || String.equal b map_or_big_map_name) -> 0
  | _ -> Ligo_string.compare ia ib

let rec assert_type_expression_eq (a, b: (type_expression * type_expression)) : unit option =
  let open Option in
  match (a.type_content, b.type_content) with
  | T_constant {language=la;injection=ia;parameters=lsta}, T_constant {language=lb;injection=ib;parameters=lstb} -> (
    if (String.equal la lb) && (constant_compare ia ib = 0) then (
      let* _ = assert_same_size lsta lstb in
      List.fold_left ~f:(fun acc p -> match acc with | None -> None | Some () -> assert_type_expression_eq p) ~init:(Some ()) (List.zip_exn lsta lstb)
    ) else
      None
  )
  | T_constant _, _ -> None
  | T_sum sa, T_sum sb -> (
      let sa' = LMap.to_kv_list_rev sa.content in
      let sb' = LMap.to_kv_list_rev sb.content in
      let aux ((ka, {associated_type=va;_}), (kb, {associated_type=vb;_})) =
        let* _ = assert_eq ka kb in
        assert_type_expression_eq (va, vb)
      in
      let* _ = assert_same_size sa' sb' in
      List.fold_left ~f:(fun acc p -> match acc with | None -> None | Some () -> aux p) ~init:(Some ()) (List.zip_exn sa' sb')
    )
  | T_sum _, _ -> None
  | T_record ra, T_record rb
       when Helpers.is_tuple_lmap ra.content <> Helpers.is_tuple_lmap rb.content -> None
  | T_record ra, T_record rb -> (
      let sort_lmap r' = List.sort ~compare:(fun (Label a,_) (Label b,_) -> String.compare a b) r' in
      let ra' = sort_lmap @@ LMap.to_kv_list_rev ra.content in
      let rb' = sort_lmap @@ LMap.to_kv_list_rev rb.content in
      let aux ((ka, {associated_type=va;_}), (kb, {associated_type=vb;_})) =
        let Label ka = ka in
        let Label kb = kb in
        let* _ = assert_eq ka kb in
        assert_type_expression_eq (va, vb)
      in
      let* _ = assert_eq ra.layout rb.layout in
      let* _ = assert_same_size ra' rb' in
      List.fold_left ~f:(fun acc p -> match acc with | None -> None | Some () -> aux p) ~init:(Some ()) (List.zip_exn ra' rb')

    )
  | T_record _, _ -> None
  | T_arrow {type1;type2}, T_arrow {type1=type1';type2=type2'} ->
    let* _ = assert_type_expression_eq (type1, type1') in
    assert_type_expression_eq (type2, type2')
  | T_arrow _, _ -> None
  | T_variable x, T_variable y ->
     (* TODO : we must check that the two types were bound at the same location (even if they have the same name), i.e. use something like De Bruijn indices or a propper graph encoding *)
     if Var.equal x y then Some () else None
  | T_variable _, _ -> None
  | T_module_accessor {module_name=mna;element=ea}, T_module_accessor {module_name=mnb;element=eb} when String.equal mna mnb ->
    assert_type_expression_eq (ea, eb)
  | T_module_accessor _, _ -> None
  | T_singleton a , T_singleton b -> assert_literal_eq (a , b)
  | T_singleton _ , _ -> None
  | T_abstraction a , T_abstraction b ->
    assert_type_expression_eq (a.type_, b.type_) >>= fun _ ->
    Some (assert (equal_kind a.kind b.kind))
  | T_for_all a , T_for_all b ->
    assert_type_expression_eq (a.type_, b.type_) >>= fun _ ->
    Some (assert (equal_kind a.kind b.kind))
  | T_abstraction _ , _ -> None
  | T_for_all _ , _ -> None

and type_expression_eq ab = Option.is_some @@ assert_type_expression_eq ab

and assert_literal_eq (a, b : literal * literal) : unit option =
  match (a, b) with
  | Literal_int a, Literal_int b when a = b -> Some ()
  | Literal_int _, Literal_int _ -> None
  | Literal_int _, _ -> None
  | Literal_nat a, Literal_nat b when a = b -> Some ()
  | Literal_nat _, Literal_nat _ -> None
  | Literal_nat _, _ -> None
  | Literal_timestamp a, Literal_timestamp b when a = b -> Some ()
  | Literal_timestamp _, Literal_timestamp _ -> None
  | Literal_timestamp _, _ -> None
  | Literal_mutez a, Literal_mutez b when a = b -> Some ()
  | Literal_mutez _, Literal_mutez _ -> None
  | Literal_mutez _, _ -> None
  | Literal_string a, Literal_string b when a = b -> Some ()
  | Literal_string _, Literal_string _ -> None
  | Literal_string _, _ -> None
  | Literal_bytes a, Literal_bytes b when a = b -> Some ()
  | Literal_bytes _, Literal_bytes _ -> None
  | Literal_bytes _, _ -> None
  | Literal_unit, Literal_unit -> Some ()
  | Literal_unit, _ -> None
  | Literal_address a, Literal_address b when a = b -> Some ()
  | Literal_address _, Literal_address _ -> None
  | Literal_address _, _ -> None
  | Literal_signature a, Literal_signature b when a = b -> Some ()
  | Literal_signature _, Literal_signature _ -> None
  | Literal_signature _, _ -> None
  | Literal_key a, Literal_key b when a = b -> Some ()
  | Literal_key _, Literal_key _ -> None
  | Literal_key _, _ -> None
  | Literal_key_hash a, Literal_key_hash b when a = b -> Some ()
  | Literal_key_hash _, Literal_key_hash _ -> None
  | Literal_key_hash _, _ -> None
  | Literal_chain_id a, Literal_chain_id b when a = b -> Some ()
  | Literal_chain_id _, Literal_chain_id _ -> None
  | Literal_chain_id _, _ -> None
  | Literal_operation _, Literal_operation _ -> None
  | Literal_operation _, _ -> None


let rec assert_value_eq (a, b: (expression*expression)) : unit option =
  let open Option in
  match (a.expression_content, b.expression_content) with
  | E_literal a, E_literal b ->
      assert_literal_eq (a, b)
  | E_constant {cons_name=ca;arguments=lsta}, E_constant {cons_name=cb;arguments=lstb} when ca = cb -> (
    let* _ = assert_same_size lsta lstb in
    List.fold_left ~f:(fun acc p -> match acc with | None -> None | Some () -> assert_value_eq p) ~init:(Some ()) (List.zip_exn lsta lstb)
  )
  | E_constant _, E_constant _ -> None
  | E_constant _, _ -> None
  | E_constructor {constructor=ca;element=a}, E_constructor {constructor=cb;element=b} when ca = cb -> (
    assert_value_eq (a, b)
  )
  | E_constructor _, E_constructor _ -> None
  | E_constructor _, _ -> None
  | E_record sma, E_record smb -> (
      let aux (Label _k) a b =
        match a, b with
        | Some a, Some b -> assert_value_eq (a, b)
        | _              -> None
      in
      let all = LMap.merge aux sma smb in
      if    ((LMap.cardinal all) = (LMap.cardinal sma))
         || ((LMap.cardinal all) = (LMap.cardinal smb)) then
        Some ()
      else None
    )
  | E_module_accessor {module_name=mna;element=a}, E_module_accessor {module_name=mnb;element=b} when String.equal mna mnb -> (
    assert_value_eq (a,b)
  )
  | E_record _, _
  | (E_literal _, _) | (E_variable _, _) | (E_application _, _)
  | (E_lambda _, _) | (E_let_in _, _) | (E_raw_code _, _) | (E_recursive _, _)
  | (E_type_in _, _)| (E_mod_in _, _) | (E_mod_alias _,_)
  | (E_record_accessor _, _) | (E_record_update _,_) | (E_type_inst _, _)
  | (E_matching _, _)
  | E_module_accessor _, _
  -> None

let merge_annotation (a:type_expression option) (b:type_expression option) assert_eq_fun : type_expression option =
  let open Option in
  match a, b with
  | None, None -> None
  | Some a, None -> Some a
  | None, Some b -> Some b
  | Some a, Some b ->
      let* _ = assert_eq_fun (a, b) in
      match a.type_meta, b.type_meta with
      | _, None -> Some a
      | _, Some _ -> Some b

let get_entry (Module_Fully_Typed lst : module_fully_typed) (name : string) : expression option =
  let aux x =
    match Location.unwrap x with
    | Declaration_constant { name = name' ; binder = _ ; expr ; attr = {inline=_ ; no_mutation = _ ; view = _ ; public = _ }} -> (
      if match name' with None -> false | Some name' -> String.equal name name'
      then Some expr
      else None
    )
    | Declaration_type   _
    | Declaration_module _
    | Module_alias       _ -> None
  in
  List.find_map ~f:aux (List.rev lst)

let equal_variables a b : bool =
  match a.expression_content, b.expression_content with
  | E_variable a, E_variable b -> Var.equal a.wrap_content b.wrap_content
  |  _, _ -> false
