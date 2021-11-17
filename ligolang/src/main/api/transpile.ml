open Api_helpers
module Compile = Ligo_compile
module Helpers   = Ligo_compile.Helpers

let contract source_file new_syntax syntax new_dialect display_format =
    Trace.warning_with @@ fun add_warning get_warnings ->
    format_result ~display_format (Parsing.Formatter.ppx_format) get_warnings @@
      fun ~raise ->
      let options         = Compiler_options.make () in
      let meta       = Compile.Of_source.extract_meta ~raise syntax source_file in
      let c_unit,_   = Compile.Utils.to_c_unit ~raise ~options ~meta source_file in
      let core       = Compile.Utils.to_core ~raise ~add_warning ~options ~meta c_unit source_file in
      let sugar      = Decompile.Of_core.decompile core in
      let imperative = Decompile.Of_sugar.decompile sugar in
      let dialect         = Decompile.Helpers.Dialect_name new_dialect in
      let buffer     =
        Decompile.Of_imperative.decompile ~raise ~dialect imperative (Syntax_name new_syntax) in
      buffer

let expression expression new_syntax syntax new_dialect display_format =
    format_result ~display_format (Parsing.Formatter.ppx_format) (fun _ -> []) @@
      fun ~raise ->
      (* Compiling chain *)
      let options            = Compiler_options.make () in
      let meta          = Compile.Of_source.make_meta ~raise syntax None in
      let c_unit_expr,_ = Compile.Of_source.compile_string ~raise ~options ~meta expression in
      let imperative    = Compile.Of_c_unit.compile_expression ~raise ~meta c_unit_expr in
      let sugar         = Compile.Of_imperative.compile_expression ~raise imperative in
      let core          = Compile.Of_sugar.compile_expression sugar in
      (* Decompiling chain *)
      let      dialect       = Decompile.Helpers.Dialect_name new_dialect in
      let n_syntax      = Decompile.Helpers.syntax_to_variant ~raise ~dialect (Syntax_name new_syntax) None in
      let sugar         = Decompile.Of_core.decompile_expression core in
      let imperative    = Decompile.Of_sugar.decompile_expression sugar in
      let buffer        = Decompile.Of_imperative.decompile_expression imperative n_syntax in
      buffer
