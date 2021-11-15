open Stage_common.Types

open Tezos_micheline.Micheline

type compiled_expression = {
  expr_ty : (Location.t, string) node ;
  expr : (Location.t, string) node ;
}

open Ligo_coq_ocaml
open Co_de_bruijn
open Ligo

val compile_expr: raise:Errors.stacking_error Trace.raise -> Environment.Protocols.t -> (Location.t, string) node list -> splitting -> (Location.t, constant', literal) expr -> (Location.t, string) node

val compile_function_body : raise:Errors.stacking_error Trace.raise -> Environment.Protocols.t -> (Location.t, constant', literal) binds -> (Location.t, string) node
