open Api_helpers
open Simple_utils
open Trace
module Compile = Ligo_compile
module Helpers   = Ligo_compile.Helpers
module Run = Ligo_run.Of_michelson

let test source_file syntax steps infer protocol_version display_format =
    Trace.warning_with @@ fun add_warning get_warnings ->
    format_result ~display_format (Ligo_interpreter.Formatter.tests_format) get_warnings @@
      fun ~raise ->
      let protocol_version = Helpers.protocol_to_variant ~raise protocol_version in
      let options = Compiler_options.make ~infer ~test:true ~protocol_version () in
      let typed,_    = Build.combined_contract ~raise ~add_warning ~options syntax source_file in
      let typed = Self_ast_typed.monomorphise_module typed in
      let steps = int_of_string steps in
      Interpreter.eval_test ~raise ~steps ~protocol_version typed

let dry_run source_file entry_point input storage amount balance sender source now syntax infer protocol_version display_format werror =
    Trace.warning_with @@ fun add_warning get_warnings ->
    format_result ~werror ~display_format (Decompile.Formatter.expression_format) get_warnings @@
      fun ~raise ->
      let protocol_version = Helpers.protocol_to_variant ~raise protocol_version in
      let options = Compiler_options.make ~infer ~protocol_version () in
      let mini_c_prg,_,typed_prg,env = Build.build_contract_use ~raise ~add_warning ~options syntax source_file in
      let michelson_prg   = Compile.Of_mini_c.aggregate_and_compile_contract ~raise ~options mini_c_prg entry_point in
      let parameter_ty =
        (* fails if the given entry point is not a valid contract *)
        let _contract = Compile.Of_michelson.build_contract ~raise michelson_prg in
        Option.map ~f:fst @@ Self_michelson.fetch_contract_ty_inputs michelson_prg.expr_ty
      in

      let compiled_params   = Compile.Utils.compile_storage ~raise ~options input storage source_file syntax env mini_c_prg in
      let args_michelson    = Run.evaluate_expression ~raise compiled_params.expr compiled_params.expr_ty in

      let options           = Run.make_dry_run_options ~raise {now ; amount ; balance ; sender ; source ; parameter_ty } in
      let runres  = Run.run_contract ~raise ~options michelson_prg.expr michelson_prg.expr_ty args_michelson in
      Decompile.Of_michelson.decompile_typed_program_entry_function_result ~raise typed_prg entry_point runres

let interpret expression init_file syntax infer protocol_version amount balance sender source now display_format =
    Trace.warning_with @@ fun add_warning get_warnings ->
    format_result ~display_format (Decompile.Formatter.expression_format) get_warnings @@
      fun ~raise ->
      let protocol_version = Helpers.protocol_to_variant ~raise protocol_version in
      let options = Compiler_options.make ~infer ~protocol_version () in
      let (decl_list,mods,env) = match init_file with
        | Some init_file ->
           let mini_c_prg,mods,_,env = Build.build_contract_use ~raise ~add_warning ~options syntax init_file in
           (mini_c_prg,mods,env)
        | None -> ([],Ast_core.SMap.empty,options.init_env) in
      let typed_exp,_    = Compile.Utils.type_expression ~raise ~options init_file syntax expression env in
      let mini_c_exp     = Compile.Of_typed.compile_expression ~raise ~module_env:mods typed_exp in
      let compiled_exp   = Compile.Of_mini_c.aggregate_and_compile_expression ~raise ~options decl_list mini_c_exp in
      let options        = Run.make_dry_run_options ~raise {now ; amount ; balance ; sender ; source ; parameter_ty = None } in
      let runres         = Run.run_expression ~raise ~options compiled_exp.expr compiled_exp.expr_ty in
      Decompile.Of_michelson.decompile_expression ~raise typed_exp.type_expression runres


let evaluate_call source_file entry_point parameter amount balance sender source now syntax infer protocol_version display_format werror =
    Trace.warning_with @@ fun add_warning get_warnings ->
    format_result ~werror ~display_format (Decompile.Formatter.expression_format) get_warnings @@
      fun ~raise ->
      let options =
        let protocol_version = Helpers.protocol_to_variant ~raise protocol_version in
        Compiler_options.make ~infer ~protocol_version ()
      in
      let mini_c_prg,mods,typed_prg,env = Build.build_contract_use ~raise ~add_warning ~options syntax source_file in
      let meta             = Compile.Of_source.extract_meta ~raise syntax source_file in
      let c_unit_param,_   = Compile.Of_source.compile_string ~raise ~options ~meta parameter in
      let imperative_param = Compile.Of_c_unit.compile_expression ~raise ~meta c_unit_param in
      let sugar_param      = Compile.Of_imperative.compile_expression ~raise imperative_param in
      let core_param       = Compile.Of_sugar.compile_expression sugar_param in
      let app              = Compile.Of_core.apply entry_point core_param in
      let typed_app,_      = Compile.Of_core.compile_expression ~raise ~options ~env app in
      let compiled_applied = Compile.Of_typed.compile_expression ~raise ~module_env:mods typed_app in

      let michelson        = Compile.Of_mini_c.aggregate_and_compile_expression ~raise ~options mini_c_prg compiled_applied in
      let options          = Run.make_dry_run_options ~raise {now ; amount ; balance ; sender ; source ; parameter_ty = None} in
      let runres           = Run.run_expression ~raise ~options michelson.expr michelson.expr_ty in
      Decompile.Of_michelson.decompile_typed_program_entry_function_result ~raise typed_prg entry_point runres

let evaluate_expr source_file entry_point amount balance sender source now syntax infer protocol_version display_format werror =
    Trace.warning_with @@ fun add_warning get_warnings ->
    format_result ~werror ~display_format Decompile.Formatter.expression_format get_warnings @@
      fun ~raise ->
      let options =
        let protocol_version = Helpers.protocol_to_variant ~raise protocol_version in
        Compiler_options.make ~infer ~protocol_version ()
      in
      let mini_c,_,typed_prg,_ = Build.build_contract_use ~raise ~add_warning ~options syntax source_file in
      let (exp,_)       = trace_option ~raise Main_errors.entrypoint_not_found @@ Mini_c.get_entry mini_c entry_point in
      let exp = Mini_c.e_var ~loc:exp.location (Location.wrap @@ Var.of_name entry_point) exp.type_expression in
      let compiled      = Compile.Of_mini_c.aggregate_and_compile_expression ~raise ~options mini_c exp in
      let options       = Run.make_dry_run_options ~raise {now ; amount ; balance ; sender ; source ; parameter_ty = None} in
      let runres        = Run.run_expression ~raise ~options compiled.expr compiled.expr_ty in
      Decompile.Of_michelson.decompile_typed_program_entry_expression_result ~raise typed_prg entry_point runres
