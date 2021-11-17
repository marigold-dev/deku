open Types

let scopes : Format.formatter -> scopes -> unit = fun f s ->
  let pp_scope f (s:scope) =
    let pp_list f = List.iter ~f: (fun (k,_) -> Format.fprintf f "%s " k ) in
    let a = Def_map.to_kv_list s.env in
    Format.fprintf f "[ %a] %a" pp_list a Location.pp s.range
  in
  let pp_scopes f = List.iter ~f: (Format.fprintf f "@[<v>%a@ @]" pp_scope) in
  Format.fprintf f "@[<v>Scopes:@ %a@]" pp_scopes s

let rec definitions : Format.formatter -> def_map -> unit = fun f dm ->
  let kvl = Def_map.to_kv_list dm in
  let pp_content ppf d = match d with
    | Variable v -> (
        let tys ppf t = match t with
          | Core t -> Format.fprintf ppf "core: %a" Ast_core.PP.type_expression t
          | Resolved t -> Format.fprintf ppf "resolved: %a" Ast_typed.PP.type_expression t
          | Unresolved -> Format.fprintf ppf "unresolved"
        in
        let refs ppf loclst =
          match loclst with
          | [] -> Format.fprintf ppf "references: []"
          | _ ->
            Format.fprintf ppf "@[<hv 2>references:@ %a@]" PP_helpers.(list_sep Location.pp (tag " ,@ ")) loclst
        in
        Format.fprintf ppf "|%a|@ %a" tys v.t refs v.references
    )
    | Type t -> Format.fprintf ppf ": %a" Ast_core.PP.type_expression t.content
    | Module m -> Format.fprintf ppf ": %a" definitions m.content
    | ModuleAlias a -> Format.fprintf ppf ": %a" (PP_helpers.list_sep_d Ast_core.PP.module_variable) @@ List.Ne.to_list a.content
  in
  let (variables,types) = List.partition_tf ~f:(fun (_,def) -> match def with Type _ | Module _ | ModuleAlias _ -> false | Variable _ -> true) kvl in
  let (types,modules) = List.partition_tf ~f:(fun (_,def) -> match def with |Type _ -> true | _ -> false) types in
  let pp_def f = List.iter ~f: (fun (k,v) -> Format.fprintf f "(%s -> %s) %a %a@ " k (get_def_name v) Location.pp (get_range v) pp_content v) in
  Format.fprintf f "@[<v>Variable definitions:@ %aType definitions:@ %aModule definitions:@ %a@]" pp_def variables pp_def types pp_def modules

let rec def_to_yojson : def -> Yojson.Safe.t = function
  | Variable { name ; range ; body_range ; t ; references } ->
    let type_case_to_yojson t = match t with
      | Core t -> `Assoc [ "core" , Ast_core.Yojson.type_expression t ]
      | Resolved t -> `Assoc [ "resolved" , Ast_typed.Yojson.type_expression t ]
      | Unresolved -> `Assoc [ "unresolved" , `Null ]
    in
    `Assoc [
      ("name", `String (Misc.get_binder_name name));
      ("range", Location.to_yojson range);
      ("body_range", Location.to_yojson body_range);
      ("t", type_case_to_yojson t );
      ("references", `List (List.map ~f:Location.to_yojson references) );
    ]
  | Type { name ; range ; body_range ; content } ->
    `Assoc [
      ("name", `String name);
      ("range", Location.to_yojson range);
      ("body_range", Location.to_yojson body_range);
      ("content", Ast_core.Yojson.type_expression content );
    ]
  | Module { name ; range ; body_range ; content } ->
    `Assoc [
      ("name", `String name);
      ("range", Location.to_yojson range);
      ("body_range", Location.to_yojson body_range);
      ("content", defs_json content );
    ]
  | ModuleAlias { name ; range ; body_range ; content } ->
    `Assoc [
      ("name", `String name);
      ("range", Location.to_yojson range);
      ("body_range", Location.to_yojson body_range);
      ("content", Stage_common.To_yojson.list Ast_core.module_variable_to_yojson @@ List.Ne.to_list content );
    ]

and defs_json d : Yojson.Safe.t =
  let get_defs d =
    let (v,tv) = List.partition_tf ~f:(fun (_,def) -> match def with Variable _ -> true | Type _ | Module _ | ModuleAlias _ -> false) (Def_map.to_kv_list d) in
    [
      ("variables", `Assoc (List.map ~f:(fun (def_id,def) -> (def_id,def_to_yojson def)) v));
      ("types", `Assoc (List.map ~f:(fun (def_id,def) -> (def_id,def_to_yojson def)) tv))
    ]
  in
  `Assoc (get_defs d)

let scopes_json s : Yojson.Safe.t = `List (
  List.map
    ~f:(fun scope ->
      let sd = Def_map.to_kv_list scope.env in
      let (variables,types) = List.partition_tf ~f:(fun (_,def) -> match def with Type _ | Module _ | ModuleAlias _ -> false | Variable _ -> true) sd in
      let v = List.map ~f:(fun (k,_) -> `String k) variables in
      let t = List.map ~f:(fun (k,_) -> `String k) types in
      (`Assoc [("range", Location.to_yojson scope.range) ; ("expression_environment", `List v) ; ("type_environment", `List t)])
    )
    s
  )

let to_json (d,s) = `Assoc [("definitions", (defs_json d)) ; ("scopes", (scopes_json s))]
