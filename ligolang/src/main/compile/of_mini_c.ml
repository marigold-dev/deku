open Main_errors
open Mini_c
open Proto_alpha_utils
open Trace
open! Stacking
open Tezos_micheline

let dummy_locations : 'l 'p. ('l, 'p) Micheline.node -> (Location.t, 'p) Micheline.node =
  fun e ->
  Micheline.(inject_locations (fun _ -> Location.dummy) (strip_locations e))

let compile_contract ~raise : options:Compiler_options.t -> expression -> Stacking.compiled_expression  = fun ~options e ->
  let e = trace ~raise self_mini_c_tracer @@ Self_mini_c.contract_check e in
  let (input_ty , _) = trace ~raise self_mini_c_tracer @@ Self_mini_c.get_t_function e.type_expression in
  let body = trace ~raise self_mini_c_tracer @@ Self_mini_c.get_function e in
  let body = Scoping.translate_closed_function body input_ty in
  let body = trace ~raise stacking_tracer @@ Stacking.Program.compile_function_body options.protocol_version body in
  let expr = Self_michelson.optimize options.protocol_version body in
  let expr_ty = Scoping.translate_type e.type_expression in
  let expr_ty = dummy_locations expr_ty in
  ({ expr_ty ; expr } : Stacking.Program.compiled_expression)

let compile_expression ~raise : options:Compiler_options.t -> expression -> compiled_expression = fun ~options e ->
  let (expr, _) = Scoping.translate_expression e [] in
  let expr = trace ~raise stacking_tracer @@ Stacking.Program.compile_expr options.protocol_version [] [] expr in
  let expr = Self_michelson.optimize options.protocol_version expr in
  let expr_ty = Scoping.translate_type e.type_expression in
  let expr_ty = dummy_locations expr_ty in
  ({ expr_ty ; expr } : Program.compiled_expression)

let compile_type = fun e ->
  let expr_ty = Scoping.translate_type e in
  dummy_locations expr_ty

let aggregate_and_compile ~raise : options:Compiler_options.t -> program -> form_t -> Stacking.compiled_expression =
    fun ~options program form ->
  let aggregated = trace ~raise self_mini_c_tracer @@ Self_mini_c.aggregate_entry program form in
  let aggregated' = Self_mini_c.all_expression ~raise aggregated in
  match form with
  | ContractForm _ -> compile_contract ~raise ~options aggregated'
  | ExpressionForm _ -> compile_expression ~raise ~options aggregated'

let aggregate_and_compile_contract ~raise : options:Compiler_options.t ->  program -> string -> Stacking.compiled_expression =
    fun ~options program name ->
  let (exp, idx) = trace_option ~raise entrypoint_not_found @@ Mini_c.get_entry program name in
  let program' = List.take program idx in
  aggregate_and_compile ~raise ~options program' (ContractForm exp)

let aggregate_and_compile_expression ~raise = fun ~options program exp ->
  aggregate_and_compile ~raise ~options program (ExpressionForm exp)

let pretty_print program = 
  Mini_c.PP.program program


(* TODO refactor? *)

let aggregate ~raise = fun program form ->
  trace ~raise self_mini_c_tracer @@
  fun ~raise ->
  let aggregated = Self_mini_c.aggregate_entry ~raise program form in
  Self_mini_c.all_expression ~raise aggregated

let aggregate_contract ~raise = fun (program : Types.program) name ->
  let (exp, idx) = trace_option ~raise entrypoint_not_found @@ get_entry program name in
  let program' = List.take program idx in
  aggregate ~raise program' (ContractForm exp)
