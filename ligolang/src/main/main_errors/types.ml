type tezos_alpha_error =  [`Tezos_alpha_error of Tezos_error_monad.Error_monad.error]

type all =
[
 | `Build_error_tracer of BuildSystem.Errors.t

 | `Main_invalid_generator_name of string
 | `Main_invalid_syntax_name of string
 | `Main_invalid_dialect_name of string
 | `Main_invalid_extension of string
 | `Main_invalid_protocol_version of string list *  string
 | `Main_invalid_typer_switch of string
 | `Main_unparse_tracer of tezos_alpha_error list
 | `Main_typecheck_contract_tracer of int Tezos_utils.Michelson.michelson * tezos_alpha_error list
 | `Main_could_not_serialize of tezos_alpha_error list
 | `Check_typed_arguments_tracer of Simple_utils.Runned_result.check_type * all
 | `Main_unknown_failwith_type
 | `Main_unknown
 | `Main_execution_failed of Simple_utils.Runned_result.failwith

 | `Unparsing_michelson_tracer of tezos_alpha_error list
 | `Parsing_payload_tracer of tezos_alpha_error list
 | `Packing_payload_tracer of tezos_alpha_error list
 | `Parsing_input_tracer of tezos_alpha_error list
 | `Parsing_code_tracer of tezos_alpha_error list
 | `Error_of_execution_tracer of tezos_alpha_error list

 | `Preproc_tracer of Preprocessing.Errors.t
 | `Parser_tracer of Parsing.Errors.t
 | `Pretty_tracer of Parsing.Errors.t
 | `Self_cst_cameligo_tracer of Self_cst.Cameligo.Errors.self_cst_cameligo_error
 | `Self_cst_pascaligo_tracer of Self_cst.Pascaligo.Errors.self_cst_pascaligo_error
 | `Self_cst_reasonligo_tracer of Self_cst.Reasonligo.Errors.self_cst_reasonligo_error
 | `Self_cst_jsligo_tracer of Self_cst.Jsligo.Errors.self_cst_jsligo_error
 | `Cit_pascaligo_tracer of Tree_abstraction.Pascaligo.Errors.abs_error
 | `Cit_cameligo_tracer of Tree_abstraction.Cameligo.Errors.abs_error
 | `Cit_reasonligo_tracer of Tree_abstraction.Reasonligo.Errors.abs_error
 | `Cit_jsligo_tracer of Tree_abstraction.Jsligo.Errors.abs_error
 | `Self_ast_imperative_tracer of Self_ast_imperative.Errors.self_ast_imperative_error
 | `Purification_tracer   of Purification.Errors.purification_error
 | `Depurification_tracer of Purification.Errors.purification_error
 | `Desugaring_tracer of Desugaring.Errors.desugaring_error
 | `Sugaring_tracer   of Desugaring.Errors.desugaring_error
 | `Inference_tracer of Inference.Errors.typer_error
 | `Checking_tracer of Checking.Errors.typer_error
 | `Self_ast_typed_tracer of Self_ast_typed.Errors.self_ast_typed_error
 | `Spilling_tracer of Spilling.Errors.spilling_error
 | `Self_mini_c_tracer of Self_mini_c.Errors.self_mini_c_error
 | `Stacking_tracer of Stacking.Errors.stacking_error

 (* | `Main_interpreter of Interpreter.interpreter_error *)
 | `Main_interpret_test_entry_not_found of string
 | `Main_interpret_target_lang_error of Location.t * Location.t list * Tezos_error_monad__TzCore.error list
 | `Main_interpret_target_lang_failwith of Location.t * Runned_result.failwith
 | `Main_interpret_boostrap_not_enough of Location.t
 | `Main_interpret_meta_lang_eval of Location.t * Location.t list * string
 | `Main_interpret_meta_lang_failwith of Location.t * Location.t list * Ligo_interpreter.Types.value
 | `Main_interpret_generic of Location.t * string
 | `Main_interpret_literal of Location.t * Ast_typed.literal
 | `Main_interpret_modules_not_supported of Location.t
 | `Main_interpret_not_enough_initial_accounts of Location.t * Memory_proto_alpha.Protocol.Alpha_context.Tez.tez

 | `Main_decompile_michelson of Stacking.Errors.stacking_error
 | `Main_decompile_mini_c of Spilling.Errors.spilling_error
 | `Main_decompile_typed of Checking.Errors.typer_error
 | `Main_decompile_inferred of Inference.Errors.typer_error
 | `Main_entrypoint_not_a_function
 | `Main_entrypoint_not_found
 | `Main_invalid_balance of string
 | `Main_invalid_amount of string
 | `Main_invalid_sender of string
 | `Main_invalid_source of string
 | `Main_invalid_timestamp of string

 | `Test_err_tracer of string * all
 | `Test_run_tracer of string * all
 | `Test_expect_tracer of Ast_core.expression * Ast_core.expression
 | `Test_expect_n_tracer of int * all
 | `Test_expect_exp_tracer of Ast_core.expression * all
 | `Test_expect_eq_n_tracer of int * all
 | `Test_internal of string
 | `Test_internal_msg of string * string
 | `Test_md_file of string * string * string * string * all
 | `Test_expected_to_fail
 | `Test_not_expected_to_fail
 | `Test_repl of string list * string list

 | `Repl_unexpected
] [@@deriving poly_constructor]
