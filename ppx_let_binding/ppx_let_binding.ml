open Ppxlib
open Ast_helper

(* on structure [%%let ("ident", value)]*)
let str_let_ident_rule =
  let expand ~ctxt name value =
    let loc = Expansion_context.Extension.extension_point_loc ctxt in
    let pattern = Pat.var ~loc { txt = name; loc } in
    [%stri let [%p pattern] = [%e value]] in

  let pattern =
    Ast_pattern.(single_expr_payload (pexp_tuple (estring __ ^:: __ ^:: nil)))
  in
  let extension =
    Extension.V3.declare "let" Extension.Context.structure_item pattern expand
  in
  Ppxlib.Context_free.Rule.extension extension

(* on signature [%%let ("ident" : typ)]*)
let sig_let_ident_rule =
  let expand ~ctxt name typ =
    let loc = Expansion_context.Extension.extension_point_loc ctxt in
    let val_desc = Val.mk ~loc { txt = name; loc } typ in
    Sig.value val_desc in

  let pattern =
    Ast_pattern.(single_expr_payload (pexp_constraint (estring __) __)) in
  let extension =
    Extension.V3.declare "let" Extension.Context.signature_item pattern expand
  in
  Ppxlib.Context_free.Rule.extension extension

(* let%ok p = e1 in e2 *)
let make_let_binding loc name var value body =
  let name = { txt = "let." ^ name; loc } in
  let binding_op = Exp.binding_op name var value loc in
  Exp.letop ~loc binding_op [] body

let make_let_extension_rule name =
  let expand ~ctxt expr =
    let loc = Expansion_context.Extension.extension_point_loc ctxt in
    match expr with
    | [%expr
        let [%p? var] = [%e? value] in
        [%e? body]] ->
      make_let_binding loc name var value body
    | _ ->
      Location.raise_errorf ~loc
        "Extension %%%s can only be used with a let, e.g. let%%%s a = b in ..."
        name name in
  let extension =
    Extension.V3.declare name Extension.Context.expression
      Ast_pattern.(single_expr_payload __)
      expand in
  Ppxlib.Context_free.Rule.extension extension

let let_extension_rules =
  List.map make_let_extension_rule ["await"; "some"; "default"; "ok"; "assert"]

let () =
  Driver.register_transformation
    ~rules:(str_let_ident_rule :: sig_let_ident_rule :: let_extension_rules)
    "ppx_let_binding"
