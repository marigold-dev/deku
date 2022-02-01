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

let () =
  Driver.register_transformation
    ~rules:[str_let_ident_rule; sig_let_ident_rule]
    "ppx_let_binding"
