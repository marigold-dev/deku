open Ppxlib
module List = ListLabels
open Ast_builder.Default

let tag = "only_interpreter"
let ppx_name = tag ^ "_tags"
let func_name = "ppx_is_" ^ tag

let constructor_impl (cd : constructor_declaration) =
  let loc = cd.pcd_loc in
  let {txt;loc=cloc} = cd.pcd_name in
  let rhs = if List.mem ~set:(List.map ~f:(fun {attr_name;_} -> attr_name.txt) cd.pcd_attributes) tag then
              [%expr true]
            else
              [%expr false] in
  let pat = match cd.pcd_args with
      | Pcstr_tuple [] | Pcstr_record [] -> None
      | _ -> Some (ppat_any ~loc) in
  case ~lhs:(ppat_construct ~loc {loc=cloc;txt=(Lident txt)} pat) ~guard:None ~rhs

let func_of_cases ~loc (cs : cases) =
  pstr_value ~loc Nonrecursive
    [ { pvb_pat = ppat_var ~loc {loc; txt = func_name}
      ; pvb_expr =
          pexp_fun ~loc Nolabel None
            (ppat_var ~loc {loc; txt = "x"})
            (pexp_match ~loc
               (pexp_ident ~loc {loc; txt = lident "x"})
               cs)
      ; pvb_attributes = []
      ; pvb_loc = loc
      }
    ]

let generate_impl ~ctxt (_rec_flag, type_declarations) =
  let loc = Expansion_context.Deriver.derived_item_loc ctxt in
  List.map type_declarations
    ~f:(fun (td : type_declaration) ->
      match td with
      | {ptype_kind = (Ptype_abstract | Ptype_record _ | Ptype_open); _} ->
        Location.raise_errorf ~loc "Cannot derive accessors for non variant types"
      | {ptype_kind = Ptype_variant constructors; _} ->
        List.map constructors ~f:constructor_impl)
  |> List.map ~f:(func_of_cases ~loc)

let impl_generator = Deriving.Generator.V2.make_noarg generate_impl

let my_deriver = Deriving.add ppx_name ~str_type_decl:impl_generator
