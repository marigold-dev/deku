open Trace
open Errors
module LT = Ligo_interpreter.Types

let mutate_some_value : raise:interpreter_error raise -> Location.t -> Z.t -> LT.value -> Ast_typed.type_expression -> (Ast_typed.expression * LT.mutation) option =
  fun ~raise loc z v v_type ->
    let n = Z.to_int z in
    let expr = Michelson_backend.val_to_ast ~raise ~loc v v_type in
    let module Fuzzer = Fuzz.Ast_typed.Mutator in
    Fuzzer.some_mutate_expression ~n expr

let mutate_all_value : raise:interpreter_error raise -> Location.t -> LT.value -> Ast_typed.type_expression -> (Ast_typed.expression * LT.mutation) list =
  fun ~raise loc v v_type ->
    let expr = Michelson_backend.val_to_ast ~raise ~loc v v_type in
    let module Fuzzer = Fuzz.Ast_typed.Mutator in
    Fuzzer.all_mutate_expression expr
