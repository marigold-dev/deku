open Api_helpers
module Compile = Ligo_compile
module Helpers   = Ligo_compile.Helpers

let pretty_print ?werror source_file syntax display_format =
    format_result ?werror ~display_format (Parsing.Formatter.ppx_format) (fun _ -> []) @@
    fun ~raise -> 
    let options = Compiler_options.make () in
    let meta = Compile.Of_source.extract_meta ~raise syntax source_file in
    Compile.Utils.pretty_print ~raise ~options ~meta source_file

let dependency_graph source_file syntax display_format =
    Trace.warning_with @@ fun add_warning get_warnings ->
    format_result ~display_format (BuildSystem.Formatter.graph_format) get_warnings @@
      fun ~raise ->
      let options = Compiler_options.make () in
      let g,_ = Build.dependency_graph ~raise ~add_warning ~options syntax Env source_file in
      (g,source_file)

let preprocess source_file syntax display_format =
    format_result ~display_format Parsing.Formatter.ppx_format (fun _ -> []) @@
    fun ~raise ->
    fst @@
    let options   = Compiler_options.make () in
    let meta = Compile.Of_source.extract_meta ~raise syntax source_file in
    Compile.Of_source.compile ~raise ~options ~meta source_file

let cst source_file syntax display_format =
    format_result ~display_format (Parsing.Formatter.ppx_format) (fun _ -> []) @@
      fun ~raise ->
      let options = Compiler_options.make () in
      let meta = Compile.Of_source.extract_meta ~raise syntax source_file in
      Compile.Utils.pretty_print_cst ~raise ~options ~meta source_file

let ast source_file syntax display_format =
    Trace.warning_with @@ fun add_warning get_warnings ->
    format_result ~display_format (Ast_imperative.Formatter.module_format) get_warnings @@
      fun ~raise ->
      let options       = Compiler_options.make () in
      let meta     = Compile.Of_source.extract_meta ~raise syntax source_file in
      let c_unit,_ = Compile.Utils.to_c_unit ~raise ~options ~meta source_file in
      Compile.Utils.to_imperative ~raise ~add_warning ~options ~meta c_unit source_file

let ast_sugar source_file syntax display_format =
    Trace.warning_with @@ fun add_warning get_warnings ->
    format_result ~display_format (Ast_sugar.Formatter.module_format) get_warnings @@
      fun ~raise ->
      let options = Compiler_options.make () in
      let meta     = Compile.Of_source.extract_meta ~raise syntax source_file in
      let c_unit,_ = Compile.Utils.to_c_unit ~raise ~options ~meta source_file in
      Compile.Utils.to_sugar ~raise ~add_warning ~options ~meta c_unit source_file

let ast_core source_file syntax infer protocol_version display_format =
    Trace.warning_with @@ fun add_warning get_warnings ->
    format_result ~display_format (Ast_core.Formatter.module_format) get_warnings @@
    fun ~raise ->
    if infer then
      (* Do the same thing as for print_ast_typed, but only infer the main module
         (it still needs to infer+typecheck the dependencies) *)
        let options = (* TODO: options should be computed outside of the API *)
          let protocol_version = Helpers.protocol_to_variant ~raise protocol_version in
          Compiler_options.make ~infer ~protocol_version ()
        in
        let inferred_core = Build.infer_contract ~raise ~add_warning ~options syntax Env source_file in
        inferred_core
    else
      (* Print the ast as-is without inferring and typechecking dependencies *)
        let options = Compiler_options.make ~infer () in
        let meta     = Compile.Of_source.extract_meta ~raise syntax source_file in
        let c_unit,_ = Compile.Utils.to_c_unit ~raise ~options ~meta source_file in
        Compile.Utils.to_core ~raise ~add_warning ~options ~meta c_unit source_file

let ast_typed source_file syntax infer protocol_version display_format =
    Trace.warning_with @@ fun add_warning get_warnings ->
    format_result ~display_format (Ast_typed.Formatter.module_format_fully_typed) get_warnings @@
    fun ~raise ->
      let options = (* TODO: options should be computed outside of the API *)
        let protocol_version = Helpers.protocol_to_variant ~raise protocol_version in
        Compiler_options.make ~infer ~protocol_version ()
      in
      let typed,_ = Build.type_contract ~raise ~add_warning ~options syntax Env source_file in
      let typed = Self_ast_typed.monomorphise_module typed in
      typed

let ast_combined  source_file syntax infer protocol_version display_format =
    Trace.warning_with @@ fun add_warning get_warnings ->
    format_result ~display_format (Ast_typed.Formatter.module_format_fully_typed) get_warnings @@
    fun ~raise ->
      let options = (* TODO: options should be computed outside of the API *)
        let protocol_version = Helpers.protocol_to_variant ~raise protocol_version in
        Compiler_options.make ~infer ~protocol_version ()
      in
      let typed,_ = Build.combined_contract ~raise ~add_warning ~options syntax source_file in
      typed

let mini_c source_file syntax infer protocol_version display_format optimize =
    Trace.warning_with @@ fun add_warning get_warnings ->
    format_result ~display_format (Mini_c.Formatter.program_format) get_warnings @@
    fun ~raise ->
      let options = (* TODO: options should be computed outside of the API *)
        let protocol_version = Helpers.protocol_to_variant ~raise protocol_version in
        Compiler_options.make ~infer ~protocol_version ()
      in
      let mini_c,_ = Build.build_mini_c ~raise ~add_warning ~options syntax Ligo_compile.Of_core.Env source_file in
      match optimize with
        | None -> Mini_c.Formatter.Raw mini_c
        | Some entry_point ->
          let o = Compile.Of_mini_c.aggregate_contract ~raise mini_c entry_point in
          Mini_c.Formatter.Optimized o
