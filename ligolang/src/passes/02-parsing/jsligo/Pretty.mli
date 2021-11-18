(* A pretty printer for JsLIGO *)

type cst       = Cst_jsligo.CST.t
type expr      = Cst_jsligo.CST.expr
type type_expr = Cst_jsligo.CST.type_expr
type pattern   = Cst_jsligo.CST.pattern

val print           : cst -> PPrint.document
val print_expr      : expr -> PPrint.document
val print_type_expr : type_expr -> PPrint.document
val print_pattern   : pattern -> PPrint.document
