open Api_helpers
open Simple_utils.Trace
module Compile = Ligo_compile
module Helpers   = Ligo_compile.Helpers

let generator_to_variant ~raise s =
  if String.equal s "list" then
    `Generator_list
  else if String.equal s "random" then
    `Generator_random
  else
    raise.raise @@ Main_errors.invalid_generator s

let mutate_ast source_file syntax infer protocol_version libs display_format seed generator =
  Trace.warning_with @@ fun add_warning get_warnings ->
  format_result ~display_format (Parsing.Formatter.ppx_format) get_warnings @@
    fun ~raise ->
    let generator = generator_to_variant ~raise generator in
    let get_module = match generator with
      | `Generator_list -> (module Fuzz.Lst : Fuzz.Monad)
      | `Generator_random -> (module Fuzz.Rnd : Fuzz.Monad) in
    let module Gen : Fuzz.Monad = (val get_module : Fuzz.Monad) in
    let module Fuzzer = Fuzz.Ast_imperative.Mutator(Gen) in
    let protocol_version = Helpers.protocol_to_variant ~raise protocol_version in
    let options       = Compiler_options.make ~infer ~protocol_version ~libs () in
    let meta     = Compile.Of_source.extract_meta ~raise syntax source_file in
    let c_unit,_ = Compile.Utils.to_c_unit ~raise ~options ~meta source_file in
    let imperative_prg = Compile.Utils.to_imperative ~raise ~add_warning ~options ~meta c_unit source_file in
    let _, imperative_prg = Fuzzer.mutate_module_ ?n:seed imperative_prg in
    let dialect         = Decompile.Helpers.Dialect_name "verbose" in
    let syntax = Helpers.variant_to_syntax meta.syntax in
    let buffer     =
        Decompile.Of_imperative.decompile ~raise ~dialect imperative_prg (Syntax_name syntax) in
    buffer

let mutate_cst source_file syntax infer protocol_version libs display_format seed generator =
  format_result ~display_format (Parsing.Formatter.ppx_format) (fun () -> []) @@
    fun ~raise ->
    let generator = generator_to_variant ~raise generator in
    let get_module = match generator with
      | `Generator_list -> (module Fuzz.Lst : Fuzz.Monad)
      | `Generator_random -> (module Fuzz.Rnd : Fuzz.Monad) in
    let module Gen : Fuzz.Monad = (val get_module : Fuzz.Monad) in
    let protocol_version = Helpers.protocol_to_variant ~raise protocol_version in
    let options   = Compiler_options.make ~infer ~protocol_version ~libs () in
    let meta     = Compile.Of_source.extract_meta ~raise syntax source_file in
    let c_unit,_ = Compile.Utils.to_c_unit ~raise ~options ~meta source_file in
    match meta with
    | {syntax = CameLIGO} ->
       begin
         let module Fuzzer = Fuzz.Cameligo.Mutator(Gen) in
         let raw =
           trace ~raise Main_errors.parser_tracer @@
             Parsing.Cameligo.parse_file c_unit source_file in
         let applied =
           trace ~raise Main_errors.self_cst_cameligo_tracer @@
             Self_cst.Cameligo.all_module raw in
         let _, mutated_prg = Fuzzer.mutate_module_ ?n:seed applied in
         let buffer = (Parsing.Cameligo.pretty_print mutated_prg) in
         buffer
       end
      | {syntax = ReasonLIGO} ->
         begin
           let module Fuzzer = Fuzz.Reasonligo.Mutator(Gen) in
           let raw =
             trace ~raise Main_errors.parser_tracer @@
               Parsing.Reasonligo.parse_file c_unit source_file in
           let applied =
             trace ~raise Main_errors.self_cst_reasonligo_tracer @@
               Self_cst.Reasonligo.all_module raw in
           let _, mutated_prg = Fuzzer.mutate_module_ ?n:seed applied in
           let buffer = (Parsing.Reasonligo.pretty_print mutated_prg) in
           buffer
         end
      | {syntax = PascaLIGO} ->
         begin
           let module Fuzzer = Fuzz.Pascaligo.Mutator(Gen) in
           let raw =
             trace ~raise Main_errors.parser_tracer @@
              Parsing.Pascaligo.parse_file c_unit source_file in
           let applied =
             trace ~raise Main_errors.self_cst_pascaligo_tracer @@
               Self_cst.Pascaligo.all_module raw in
           let _, mutated_prg = Fuzzer.mutate_module_ ?n:seed applied in
           let buffer = (Parsing.Pascaligo.pretty_print mutated_prg) in
           buffer
         end
      | {syntax = JsLIGO} ->
         begin
           let module Fuzzer = Fuzz.Jsligo.Mutator(Gen) in
           let raw =
            trace ~raise Main_errors.parser_tracer @@
              Parsing.Jsligo.parse_file c_unit source_file in
           let applied =
             trace ~raise Main_errors.self_cst_jsligo_tracer @@
               Self_cst.Jsligo.all_module raw in
           let _, mutated_prg = Fuzzer.mutate_module_ ?n:seed applied in
           let buffer = (Parsing.Jsligo.pretty_print mutated_prg) in
           buffer
         end
