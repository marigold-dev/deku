open Base
open Ppxlib
open Ast_builder.Default

let raise_unsupported ~loc = Location.raise_errorf ~loc "ppx_poly_constructor: unsupported."

module Helper = struct
  let struct_string_opt s =
    match s with
    | [{ pstr_desc = Pstr_eval ({ pexp_desc = Pexp_constant (Pconst_string (s, _, _)) ; _ }, _) ; _}] -> Some s
    | _ -> None

  let payload_of_attributes_opt tag attributes =
    let f { attr_name ; attr_payload ; _ } =
      match attr_payload with
      | PStr s when String.equal attr_name.txt tag -> struct_string_opt s
      | _ -> None in
    List.find_map attributes ~f

  let protect_keyword s = if Keyword.is_keyword s then s ^ "_" else s
end

module Constructor = struct
  type 'a t = { constructor : string ; payload : 'a ; loc : Location.t ; args : core_type option }

  let to_args t = match t.args with
    | None -> []
    | Some { ptyp_desc = Ptyp_tuple l ; _ } -> List.mapi l ~f:(fun i _ -> (Nolabel, "a" ^ (Int.to_string i)))
    | Some _ -> [Nolabel, "a0"]
end

module type PAYLOAD_EXTRACT = sig
  type t
  val payload_of_attributes_opt : attributes -> t
end

module Inspector(PE : PAYLOAD_EXTRACT) = struct
  let row_field ~loc rf : PE.t Constructor.t =
    match rf.prf_desc with
    | Rtag ({ txt = constructor ; _ }, true, _) | Rtag ({ txt = constructor ; _ }, _, []) ->
      { constructor ; payload = PE.payload_of_attributes_opt rf.prf_attributes ; loc ; args = None }
    | Rtag ({ txt = constructor ; _ }, false, tp :: _) ->
      { constructor ; payload = PE.payload_of_attributes_opt rf.prf_attributes ; loc ; args = Some tp }
    | Rinherit _ -> raise_unsupported ~loc

  let type_declaration td =
    let loc = td.ptype_loc in
    match td.ptype_kind with
    | Ptype_variant _ | Ptype_record _ | Ptype_open -> raise_unsupported ~loc
    | Ptype_abstract ->
      match td.ptype_manifest with
      | Some { ptyp_desc = Ptyp_variant (rfs, Closed, None); _ } -> List.map rfs ~f:(row_field ~loc)
      | _ -> raise_unsupported ~loc
end

module Gen_str = struct
  let constructor_to_string ~prefix s =
    let s = String.lowercase s in
    let s = Option.value ~default:s @@ Option.bind prefix ~f:(fun prefix -> String.chop_prefix ~prefix s) in
    Helper.protect_keyword s

  let constructors ?prefix loc constructors =
    let open Constructor in
    List.map constructors ~f:(fun c ->
        let name = Option.value ~default:(constructor_to_string ~prefix c.constructor) c.payload in
        let args = to_args c in
        let constructed_value =
          let arg = pexp_tuple_opt ~loc (List.map args ~f:(fun (_, v) -> evar ~loc v)) in
          pexp_variant ~loc c.constructor arg in
        let expr = (List.fold_right args ~init:constructed_value ~f:(fun (label, v) e -> pexp_fun ~loc label None (pvar ~loc v) e)) in
        let value_binding = value_binding ~loc ~pat:(pvar ~loc name) ~expr in
        pstr_value ~loc Nonrecursive [ value_binding ])

  let generate ~loc ~path:_ (_, tds) prefix =
    let module TE = struct
        type t = string option
        let payload_of_attributes_opt = Helper.payload_of_attributes_opt "name"
      end in
    let module I = Inspector(TE) in
    match tds with
    | [td] ->
       let prefix = match prefix with
         | Some { pexp_desc = Pexp_constant (Pconst_string (s, _, _)) ; _ } -> Some s
         | _ -> None in
       constructors ?prefix td.ptype_loc (I.type_declaration td)
    | _ -> raise_unsupported ~loc
end

let _ = let prefix = Deriving.Args.(empty +> arg "prefix" __) in
        Deriving.add "poly_constructor" ~str_type_decl:(Deriving.Generator.make prefix Gen_str.generate)
