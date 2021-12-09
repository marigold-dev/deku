open Zincing
open Main_errors
open Trace
module SMap = Map.Make (String)

let compile_with_modules ~raise ?module_env:_ :
    Ast_typed.module_fully_typed -> Zinc_types.Raw.Program.t =
 fun p ->
  (* trace ~raise spilling_tracer @@ compile_module ~module_env:module_env p *)
  trace ~raise spilling_tracer @@ compile_module p

let compile ~raise ?(module_env = SMap.empty) :
    Ast_typed.module_fully_typed -> Zinc_types.Raw.Program.t =
 fun p ->
  (*let zinc,_ = compile_with_modules ~raise ~module_env:module_env p in
    zinc*)
  let zinc = compile_with_modules ~raise ~module_env p in
  zinc
