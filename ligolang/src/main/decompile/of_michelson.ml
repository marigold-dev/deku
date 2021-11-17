module Formatter = Formatter

open Main_errors
open Trace
open Simple_utils.Runned_result

type ret_type = Function | Expression
let decompile_value ~raise func_or_expr program entry (ty, value) =
  let output_type =
    let entry_expression = trace_option ~raise entrypoint_not_found @@ Ast_typed.get_entry program entry in
    match func_or_expr with
    | Expression ->
      entry_expression.type_expression
    | Function ->
      let (_,output_type) = trace_option ~raise entrypoint_not_a_function @@ Ast_typed.get_t_function entry_expression.type_expression in
      output_type in
  let mini_c = trace ~raise decompile_michelson @@ Stacking.Decompiler.decompile_value ty value in
  let typed =  trace ~raise decompile_mini_c    @@ Spilling.decompile mini_c output_type in
  let inferred  = Checking.untype_expression typed in
  let core      = Inference.Untyper.untype_expression inferred in
  core

let decompile_typed_program_entry_expression_result ~raise program entry runned_result =
  match runned_result with
  | Fail s -> Fail s
  | Success ex_ty_value ->
    let decompiled_value = decompile_value ~raise Expression program entry ex_ty_value in
    (Success decompiled_value)

let decompile_typed_program_entry_function_result ~raise program entry runned_result =
  match runned_result with
  | Fail s -> Fail s
  | Success ex_ty_value ->
    let decompiled_value = decompile_value ~raise Function program entry ex_ty_value in
    (Success decompiled_value)

let decompile_expression ~raise type_value runned_result =
  match runned_result with
  | Fail s -> Fail s
  | Success (ty, value) ->
    let mini_c = trace ~raise decompile_michelson @@ Stacking.Decompiler.decompile_value ty value in
    let typed = trace ~raise decompile_mini_c @@ Spilling.decompile mini_c type_value in
    let decompiled_value = Checking.untype_expression typed in
    (Success decompiled_value)
