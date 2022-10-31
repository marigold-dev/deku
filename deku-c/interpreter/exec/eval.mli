open Values
open Instance

exception Link of Source.region * string
exception Trap of Source.region * string
exception Crash of Source.region * string
exception Exhaustion of Source.region * string

val init :
  Ast.module_ ->
  (Utf8.t * extern) list ->
  gas_limit:I64.t ->
  module_inst (* raises Link, Trap *)

val invoke : func_inst -> value list -> value list (* raises Trap *)
