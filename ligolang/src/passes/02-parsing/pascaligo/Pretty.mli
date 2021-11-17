(* A pretty printer for PascaLIGO *)

module CST = Cst_pascaligo.CST

type cst       = CST.t
type expr      = CST.expr
type type_expr = CST.type_expr
type pattern   = CST.pattern

val print           : cst -> PPrint.document
val print_expr      : expr -> PPrint.document
val print_type_expr : type_expr -> PPrint.document
val print_pattern   : pattern -> PPrint.document
