open Tezos_clic
open Cli_helpers

let is_dev = ref false

let source_file : type a. (a,'ctx) Clic.params -> (string -> a, 'ctx) Clic.params =
  fun a ->
  let name = "SOURCE_FILE" in
  let desc = name ^ " is the path to the smart contract file." in
  Clic.string ~name ~desc a

let entry_point =
  let docv = "ENTRY_POINT" in
  let doc = "the entry-point that will be compiled." in
  Clic.default_arg ~doc ~short:'e' ~long:"entry-point" ~placeholder:docv ~default:"main" @@
  Clic.parameter @@
  fun _ s -> Proto_alpha_utils.Error_monad.return s

let expression purpose =
  let name = purpose ^ "_EXPRESSION" in
  let desc = "the expression that will be compiled." in
  Clic.string ~name ~desc

let libraries : (string list, _) Clic.arg =
  let docv = "LIBRARY" in
  let doc = "A list of path to a directory containing included files, separated by ',' " in
  Clic.default_arg ~doc ~short:'l' ~long:"lib" ~placeholder:docv ~default:"" @@
  Clic.parameter @@
  fun _ s -> Proto_alpha_utils.Error_monad.return @@ Base.String.split ~on:',' s

let syntax =
  let docv = "SYNTAX" in
  let doc = "the syntax that will be used. Currently supported syntaxes are \"pascaligo\", \"cameligo\", \"reasonligo\" and \"jsligo\". By default, the syntax is guessed from the extension (.ligo, .mligo, .religo, and .jsligo respectively)." in
  Clic.default_arg ~doc ~short:'s' ~long:"syntax" ~placeholder:docv ~default:"auto" @@
  Clic.parameter @@
  fun _ s -> Proto_alpha_utils.Error_monad.return s

let on_chain_views : (string list, _) Clic.arg =
  let docv = "ON_CHAIN_VIEWS" in
  let doc = "A list of declaration name that will be compiled as on-chain views, separated by ','" in
  Clic.default_arg ~doc ~short:'v' ~long:"views" ~placeholder:docv ~default:"" @@
  Clic.parameter @@
  fun _ s -> Proto_alpha_utils.Error_monad.return (
    match s with
    | "" -> []
    | _ -> Base.String.split ~on:',' s
  )

let steps =
  let docv = "STEPS" in
  let doc = "a bound in the number of steps to be done by the interpreter." in
  Clic.default_arg ~doc ~short:'n' ~long:"steps" ~placeholder:docv ~default:"1000000" @@
  Clic.parameter @@
  fun _ s -> Proto_alpha_utils.Error_monad.return s

let protocol_version =
  let open Environment.Protocols in
  let plist = Format.asprintf "%a" (Simple_utils.PP_helpers.list_sep_d_par Format.pp_print_string) protocols_str in
  let docv = "PROTOCOL_VERSION" in
  let doc = Format.asprintf "Choose protocol's types/values pre-loaded into the LIGO environment %s. \
                              By default, the current protocol (%s) will be used" plist (variant_to_string current) in
  Clic.default_arg ~doc ~short:'p' ~long:"protocol" ~placeholder:docv ~default:"current" @@
  Clic.parameter @@
  fun _ s -> Proto_alpha_utils.Error_monad.return s

let dialect =
  let docv = "PASCALIGO_DIALECT" in
  let doc = "the pascaligo dialect that will be used. Currently supported dialects are \"terse\" and \"verbose\". By default the dialect is \"terse\"." in
  Clic.default_arg ~doc ~short:'d' ~long:"dialect" ~placeholder:docv ~default:"terse" @@
  Clic.parameter @@
  fun _ s -> Proto_alpha_utils.Error_monad.return s

let req_syntax =
  fun a ->
  let name = "SYNTAX" in
  let desc = "the syntax that will be used. Currently supported syntaxes are \"pascaligo\", \"cameligo\" and \"reasonligo\". By default, the syntax is guessed from the extension (.ligo, .mligo, .religo, .jsligo respectively)." in
  Clic.string ~name ~desc a
let init_file =
  let docv = "INIT_FILE" in
  let doc = "The path to the smart contract file to be used for context initialization." in
  Clic.arg ~doc ~long:"init-file" ~placeholder:docv @@
  Clic.parameter @@
  fun _ s -> Proto_alpha_utils.Error_monad.return s

let amount =
  let docv = "AMOUNT" in
  let doc = "The tezos amount the Michelson interpreter will use for the transaction." in
  Clic.default_arg ~doc ~long:"amount" ~placeholder:docv ~default:"0" @@
  Clic.parameter @@
  fun _ s -> Proto_alpha_utils.Error_monad.return s


let balance =
  let docv = "BALANCE" in
  let doc = "The balance the Michelson interpreter will use for the contract balance." in
  Clic.default_arg ~doc ~long:"balance" ~placeholder:docv ~default:"0" @@
  Clic.parameter @@
  fun _ s -> Proto_alpha_utils.Error_monad.return s

let sender =
  let docv = "SENDER" in
  let doc = "The sender the Michelson interpreter transaction will use." in
  Clic.arg ~doc ~long:"sender" ~placeholder:docv @@
  Clic.parameter @@
  fun _ s -> Proto_alpha_utils.Error_monad.return s

let source =
  let docv = "SOURCE" in
  let doc = "The source the Michelson interpreter transaction will use." in
  Clic.arg ~doc ~long:"source" ~placeholder:docv @@
  Clic.parameter @@
  fun _ s -> Proto_alpha_utils.Error_monad.return s

let disable_michelson_typechecking =
  let doc = "Disable Michelson typecking, this might produce ill-typed Michelson code." in
  Clic.switch ~doc ~long:"disable-michelson-typechecking" ()

let without_run =
  let doc = "disable running of compiled expression." in
  Clic.switch ~doc ~long:"without-run" ()

let with_types =
  let doc = "Tries to infer types for all named expressions" in
  Clic.switch ~doc ~long:"with-types" ()

let now =
  let docv = "NOW" in
  let doc = "The NOW value the Michelson interpreter will use (e.g. '2000-01-01T10:10:10Z')" in
  Clic.arg ~doc ~long:"now" ~placeholder:docv @@
  Clic.parameter @@
  fun _ s -> Proto_alpha_utils.Error_monad.return s

let display_format =
  let open Display in
  let docv = "DISPLAY-FORMAT" in
  let doc = "The format that will be used by the CLI. Available formats are 'dev', 'json', and 'human-readable' (default). When human-readable lacks details (we are still tweaking it), please contact us and use another format in the meanwhile." in
  Clic.default_arg ~doc ~long:"format" ~placeholder:docv ~default:"human-readable" @@
  Clic.parameter @@
  fun _ s -> match s with
    | "human-readable" -> Proto_alpha_utils.Error_monad.return human_readable
    | "dev"            -> let () = is_dev := true in Proto_alpha_utils.Error_monad.return dev
    | "json"           -> Proto_alpha_utils.Error_monad.return json
    | _ -> failwith "todo"

let output_file =
  let docv = "OUTPUT_FILE" in
  let doc = "If used, prints the output into the specified file instead of stdout" in
  Clic.arg ~doc ~short:'o' ~long:"output-file" ~placeholder:docv @@
  Clic.parameter @@
  fun _ s -> Proto_alpha_utils.Error_monad.return s

let michelson_code_format =
  let docv = "MICHELSON_FORMAT" in
  let doc = "Is the format that will be used by compile contract for the resulting Michelson. Available formats are 'text' (default), 'json' and 'hex'." in
  Clic.default_arg ~doc ~long:"michelson-format" ~placeholder:docv ~default:"text" @@
  Clic.parameter @@
  fun _ s -> match s with
    | "text" -> Proto_alpha_utils.Error_monad.return `Text
    | "json" -> Proto_alpha_utils.Error_monad.return `Json
    | "hex"  -> Proto_alpha_utils.Error_monad.return `Hex
    | _ -> failwith "todo"

let optimize =
  let docv = "ENTRY_POINT" in
  let doc = "Apply Mini-C optimizations as if compiling for this entry_point" in
  Clic.arg ~doc ~long:"optimize" ~placeholder:docv @@
  Clic.parameter @@
  fun _ s -> Proto_alpha_utils.Error_monad.return s

let infer =
  let doc = "enable type inference" in
  Clic.switch ~doc ~long:"infer" ()

let warn =
  let docv = "BOOL" in
  let doc = "Indicates whether warning messages should be printed in stderr or not" in
  Clic.default_arg ~doc ~long:"warn" ~placeholder:docv ~default:"true" @@
  Clic.parameter @@
  fun _ s -> match s with
    | "true"  -> Proto_alpha_utils.Error_monad.return true
    | "false" -> Proto_alpha_utils.Error_monad.return false
    | _ -> failwith "todo"

let werror =
  let docv = "BOOL" in
  let doc = "Indicates whether warning messages should be treated as errors or not" in
  Clic.default_arg ~doc ~long:"werror" ~placeholder:docv ~default:"false" @@
  Clic.parameter @@
  fun _ s -> match s with
    | "true"  -> Proto_alpha_utils.Error_monad.return true
    | "false" -> Proto_alpha_utils.Error_monad.return false
    | _ -> failwith "todo"

let seed =
  let docv = "SEED" in
  let doc = "Is the seed or counter used for generation." in
  Clic.arg ~doc ~long:"seed" ~placeholder:docv @@
  Clic.parameter @@
  fun _ s ->
    Proto_alpha_utils.Error_monad.return @@
    Base.Int.of_string s

let generator =
  let docv = "GENERATOR" in
  let doc = "Is the generator for mutation." in
  Clic.default_arg ~doc ~short:'g' ~long:"generator" ~placeholder:docv ~default:"random" @@
  Clic.parameter @@
  fun _ s -> Proto_alpha_utils.Error_monad.return s

let global_options = Clic.no_options

module Api = Ligo_api

let compile_group = Clic.{name="compile";title="Commands for compiling from Ligo to Michelson"}
let compile_file =
  let f (entry_point, oc_views, syntax, infer, protocol_version, display_format, disable_typecheck, michelson_format, output_file, warn, werror) source_file () =
    return_result ~warn ?output_file @@
    Api.Compile.contract ~werror source_file entry_point oc_views syntax infer protocol_version display_format disable_typecheck michelson_format in
  let _doc = "Subcommand: Compile a contract." in
  let desc =     "This sub-command compiles a contract to Michelson \
                 code. It expects a source file and an entrypoint \
                 function that has the type of a contract: \"parameter \
                 * storage -> operations list * storage\"." in
  Clic.command

    ~group:compile_group
    ~desc
    Clic.(args11 entry_point on_chain_views syntax infer protocol_version display_format disable_michelson_typechecking michelson_code_format output_file warn werror)
    Clic.(prefixes ["compile"; "contract"] @@ source_file @@ stop)
    f

let compile_parameter =
  let f (entry_point, syntax, infer, protocol_version, amount, balance, sender, source, now, display_format, michelson_format, output_file, warn, werror) source_file expression () =
    return_result ~warn ?output_file @@
    Api.Compile.parameter source_file entry_point expression syntax infer protocol_version amount balance sender source now display_format michelson_format werror
    in
  let _cmdname = "compile-parameter" in
  let _doc = "Subcommand: Compile parameters to a Michelson expression." in
  let desc =     "This sub-command compiles a parameter for a given \
                 contract to a Michelson expression. The resulting \
                 Michelson expression can be passed as an argument in \
                 a transaction which calls a contract." in
  Clic.command
    ~group:compile_group
    ~desc
    Clic.(args14 entry_point syntax infer protocol_version amount balance sender source now display_format michelson_code_format output_file warn werror)
    Clic.(prefixes ["compile"; "parameter"] @@ source_file @@ expression "PARAMETER" @@ stop)
    f

let compile_expression =
  let f (infer, protocol_version, init_file, display_format, without_run, michelson_format, warn, werror) syntax expression () =
    return_result ~warn @@
    Api.Compile.expression expression syntax infer protocol_version init_file display_format without_run michelson_format werror
    in
  let _cmdname = "compile-expression" in
  let _doc = "Subcommand: Compile to a Michelson value." in
  let desc =     "This sub-command compiles a LIGO expression to a \
                 Michelson value. It works by compiling the LIGO \
                 expression to a Michelson expression and then \
                 interpreting it using Michelson's interpreter." in
  Clic.command
    ~group:compile_group
    ~desc
    Clic.(args8 infer protocol_version init_file display_format without_run michelson_code_format warn werror)
    Clic.(prefixes ["compile"; "expression"] @@ req_syntax @@ expression "" @@ stop)
    f

let compile_storage =
  let f (entry_point, syntax, infer, protocol_version, amount, balance, sender, source, now, display_format, michelson_format, output_file, warn, werror) source_file expression () =
    return_result ~warn ?output_file @@
    Api.Compile.storage source_file entry_point expression syntax infer protocol_version amount balance sender source now display_format michelson_format werror
  in
  let _cmdname = "compile-storage" in
  let _doc = "Subcommand: Compile an initial storage in LIGO syntax to \
             a Michelson expression." in
  let desc =     "This sub-command compiles an initial storage for a \
                 given contract to a Michelson expression. The \
                 resulting Michelson expression can be passed as an \
                 argument in a transaction which originates a contract." in
  Clic.command
    ~group:compile_group
    ~desc
    Clic.(args14 entry_point syntax infer protocol_version amount balance sender source now display_format michelson_code_format output_file warn werror)
    Clic.(prefixes ["compile";"storage"] @@ source_file @@ expression "STORAGE" @@ stop)
    f

(** Transpile commands *)
let transpile_group = Clic.{name="transpile";title="Commands for transpiling from the different Ligo syntaxes"}
let transpile_contract =
  let f (syntax, new_dialect, display_format, output_file) source_file new_syntax () =
    return_result ?output_file @@
    Api.Transpile.contract source_file new_syntax syntax new_dialect display_format
  in
  let _doc = "Subcommand: Transpile a contract to another syntax (BETA)." in
  let desc =     "This sub-command transpiles a source file to another \
                 syntax. It does not use the build system, but the \
                 source file is preprocessed. Comments are currently \
                 not transpiled. Please use at your own risk." in
  Clic.command
    ~group:transpile_group
    ~desc
    Clic.(args4 syntax dialect display_format output_file)
    Clic.(prefixes ["transpile";"contract"] @@ source_file @@ req_syntax @@ stop)
    f


let transpile_expression =
  let f (new_dialect, display_format) syntax expression new_syntax () =
    return_result @@
    Api.Transpile.expression expression new_syntax syntax new_dialect display_format
  in
  let _doc = "Subcommand: Transpile an expression to another syntax (BETA)." in
  let desc =     "This sub-command transpiles a LIGO expression to \
                 another syntax. Comments are currently not \
                 transpiled. Please use at your own risk." in
  Clic.command ~group:transpile_group ~desc
    Clic.(args2 dialect display_format)
    Clic.(prefixes ["transpile";"expression"] @@ req_syntax @@ expression "" @@ req_syntax @@ stop)
    f


(** Mutate commands *)
let mutate_group = Clic.{name="mutate";title="Commands for creating mutant of a contracts"}
let mutate_cst =
  let f (syntax, infer, protocol_version, libs, display_format, seed, generator) source_file () =
    return_result @@
    Api.Mutate.mutate_cst source_file syntax infer protocol_version libs display_format seed generator in
  let _doc = "Subcommand: Return a mutated version for a given file." in
  let desc =    "This sub-command returns a mutated version for a \
                 given file. It does not use the build system." in
  Clic.command ~group:mutate_group ~desc
    Clic.(args7 syntax infer protocol_version libraries display_format seed generator)
    Clic.(prefixes ["mutate";"cst"] @@ source_file @@ stop)
    f

let mutate_ast =
  let f (syntax, infer, protocol_version, libs, display_format, seed, generator) source_file () =
    return_result @@
    Api.Mutate.mutate_ast source_file syntax infer protocol_version libs display_format seed generator
  in
  let _doc = "Subcommand: Return a mutated version for a given file." in
  let desc =    "This sub-command returns a mutated version for a \
                 given file. It does not use the build system." in
  Clic.command ~group:mutate_group ~desc
    Clic.(args7 syntax infer protocol_version libraries display_format seed generator)
    Clic.(prefixes ["mutate";"ast"] @@ source_file @@ stop)
    f

(** Run commands *)
let run_group = Clic.{name="run";title="Commands for executing Ligo code"}
let test =
  let f (syntax, steps, infer, protocol_version, display_format) source_file () =
    return_result @@
    Api.Run.test source_file syntax steps infer protocol_version display_format
  in
  let _doc = "Subcommand: Test a contract with the LIGO test framework (BETA)." in
  let desc =    "This sub-command tests a LIGO contract using a LIGO \
                 interpreter. Still under development, there are features that are work \
                 in progress and are subject to change. No real test \
                 procedure should rely on this sub-command alone."
  in
  Clic.command ~group:run_group ~desc
    Clic.(args5 syntax steps infer protocol_version display_format)
    Clic.(prefixes ["run";"test"] @@ source_file @@ stop)
    f

let dry_run =
  let f (entry_point, amount, balance, sender, source, now, syntax, infer, protocol_version, display_format, warn, werror) source_file input storage () =
    return_result ~warn @@
    Api.Run.dry_run source_file entry_point input storage amount balance sender source now syntax infer protocol_version display_format werror
    in
  let _doc = "Subcommand: Run a smart-contract with the given storage and input." in
  let desc =     "This sub-command runs a LIGO contract on a given \
                 storage and parameter. The context is initialized \
                 from a source file where the contract is \
                 implemented. The interpretation is done using \
                 Michelson's interpreter." in
  Clic.command ~group:run_group ~desc
    Clic.(args12 entry_point amount balance sender source now syntax infer protocol_version display_format warn werror)
    Clic.(prefixes ["run";"dry-run"] @@ source_file @@ expression "PARAMETER" @@ expression "STORAGE" @@ stop)
    f

let evaluate_call ~cmdname_deprecation =
  let f (entry_point, amount, balance, sender, source, now, syntax, infer, protocol_version, display_format, warn, werror) source_file parameter () =
    return_result ~warn @@
    Api.Run.evaluate_call source_file entry_point parameter amount balance sender source now syntax infer protocol_version display_format werror
    in
  (* "run-function" was renamed to "evaluate-call", keeping both for a few versions for backward-compatibility. *)
  let cmdname = match cmdname_deprecation with
  | `deprecated_run_function -> "run-function"
  | `evaluate_call -> "evaluate-call" in
  let deprecation = match cmdname_deprecation with
  | `deprecated_run_function -> "Deprecated, renamed to evaluate-call. Use evaluate-call instead. "
  | `evaluate_call -> "" in
  let _doc = deprecation ^ "Subcommand: Run a function with the given parameter." in
  let desc = (deprecation ^
                 "This sub-command runs a LIGO function on a given \
                  argument. The context is initialized from a source \
                  file where the function is implemented. The \
                  interpretation is done using Michelson's interpreter.") in
  Clic.command ~group:run_group ~desc
    Clic.(args12 entry_point amount balance sender source now syntax infer protocol_version display_format warn werror)
    Clic.(prefixes ["run";cmdname] @@ source_file @@ expression "PARAMETER" @@ stop)
    f

let evaluate_expr ~cmdname_deprecation =
  let f (entry_point, amount, balance, sender, source, now, syntax, infer, protocol_version, display_format, warn, werror) source_file () =
    return_result ~warn @@
    Api.Run.evaluate_expr source_file entry_point amount balance sender source now syntax infer protocol_version display_format werror
    in
  (* "run-function" was renamed to "evaluate-call", keeping both for a few versions for backward-compatibility. *)
  let cmdname = match cmdname_deprecation with
  | `deprecated_evaluate_value -> "evaluate-value"
  | `evaluate_expr -> "evaluate-expr" in
  let deprecation = match cmdname_deprecation with
  | `deprecated_evaluate_value -> "Deprecated, renamed to evaluate-expr. Use evaluate-expr instead. "
  | `evaluate_expr -> "" in
  let _doc = deprecation ^ "Subcommand: Evaluate a given definition." in
  let desc = (deprecation ^
                 "This sub-command evaluates a LIGO definition. The \
                  context is initialized from a source file where the \
                  definition is written. The interpretation is done \
                  using a Michelson interpreter.") in
  Clic.command ~group:run_group ~desc
    Clic.(args12 entry_point amount balance sender source now syntax infer protocol_version display_format warn werror)
    Clic.(prefixes ["run";cmdname] @@ source_file @@ stop)
    f

let interpret =
  let f (init_file, syntax, infer, protocol_version, amount, balance, sender, source, now, display_format) expression () =
    return_result @@
    Api.Run.interpret expression init_file syntax infer protocol_version amount balance sender source now display_format
  in
  let _doc = "Subcommand: Interpret the expression in the context initialized by the provided source file." in
  let desc = "This sub-command interprets a LIGO expression. The \
                 context can be initialized by providing a source \
                 file. The interpretation is done using Michelson's \
                 interpreter." in
  Clic.command ~group:run_group ~desc
    Clic.(args10 init_file syntax infer protocol_version amount balance sender source now display_format)
    Clic.(prefixes ["run";"interpret"] @@ expression "EXPRESSION" @@ stop)
    f

(** Info commands *)
let info_group = Clic.{name="info";title="Commands to get information from contracts"}

let list_declarations =
  let f (syntax, display_format) source_file () =
    return_result @@
    Api.Info.list_declarations source_file syntax display_format
  in
  let _doc = "Subcommand: List all the top-level declarations." in
  let desc =    "This sub-command prints a list of all top-level \
                 declarations (not including types and modules)." in
  Clic.command ~group:info_group ~desc
    Clic.(args2 syntax display_format)
    Clic.(prefixes ["info"; "list-declarations"] @@ source_file @@ stop)
    f

let measure_contract =
  let f (entry_point, oc_views, syntax, infer, protocol_version, display_format, warn, werror) source_file () =
    return_result ~warn @@
    Api.Info.measure_contract source_file entry_point oc_views syntax infer protocol_version display_format werror
  in
  let _doc = "Subcommand: Measure a contract's compiled size in bytes." in
  let desc =    "This sub-command compiles a source file and measures \
                 the contract's compiled size in bytes." in
  Clic.command ~group:info_group ~desc
    Clic.(args8 entry_point on_chain_views syntax infer protocol_version display_format warn werror)
    Clic.(prefixes ["info";"measure-contract"] @@ source_file @@ stop)
    f

let get_scope =
  let f (syntax, infer, protocol_version, libs, display_format, with_types) source_file () =
    return_result @@
    Api.Info.get_scope source_file syntax infer protocol_version libs display_format with_types
  in
  let _doc = "Subcommand: Return the JSON encoded environment for a given file." in
  let desc =     "This sub-command returns the environment for a given \
                 file in JSON format. It does not use the build system." in
  Clic.command ~group:info_group ~desc
    Clic.(args6 syntax infer protocol_version libraries display_format with_types)
    Clic.(prefixes ["info";"get-scope"] @@ source_file @@ stop)
    f


(** Print commands *)
let print_group = Clic.{name="print";title="print title"}
let preprocessed =
  let f (syntax, display_format) source_file () =
    return_result @@
      Api.Print.preprocess source_file syntax display_format in
  let _cmdname = "preprocessed" in
  let _doc = "Subcommand: Preprocess the source file.\nWarning: Intended for development of LIGO and can break at any time." in
  let desc =    "This sub-command runs the pre-processor on a LIGO \
                 source file and outputs the result. The directive \
                 `#include` directly inlines the included file and \
                 therefore its content appears in the output. In \
                 contrast, the directive `#import` includes the file \
                 as a module and therefore the content of the imported \
                 file is not printed by this sub-command." in
  Clic.command
    ~group:print_group
    ~desc
    Clic.(args2 syntax display_format)
    Clic.(prefixes ["print"; "preprocessed"] @@ source_file @@ stop)
    f

let pretty_print =
  let f (syntax, display_format) source_file () =
    return_result @@
    Api.Print.pretty_print source_file syntax display_format in
  let _doc = "Subcommand: Pretty-print the source file." in
  let desc =     "This sub-command pretty-prints a source file in \
                 LIGO. The width of the pretty-printed text is \
                 adjusted to the number of columns in the terminal (or \
                 60 if it cannot be determined)." in
  Clic.command
    ~group:print_group
    ~desc
    Clic.(args2 syntax display_format)
    Clic.(prefixes ["print";"pretty-print"] @@ source_file @@ stop)
    f

let print_graph =
  let f (syntax, display_format) source_file () =
    return_result @@
    Api.Print.dependency_graph source_file syntax display_format
  in
  let _cmdname = "print-graph" in
  let _doc = "Subcommand: Print the dependency graph.\nWarning: Intended for development of LIGO and can break at any time." in
  let desc =     "This sub-command prints the dependency graph created \
                 by the module system. It explores all imported source \
                 files (recursively) following a DFS strategy." in
  Clic.command
    ~group:print_group
    ~desc
    Clic.(args2 syntax display_format)
    Clic.(prefixes ["print";"dependency-graph"] @@ source_file @@ stop)
    f

let print_cst =
  let f (syntax, display_format) source_file () =
    return_result @@
    Api.Print.cst source_file syntax display_format
  in
  let _cmdname = "print-cst" in
  let _doc = "Subcommand: Print the CST.\nWarning: Intended for development of LIGO and can break at any time." in
  let desc =     "This sub-command prints the source file in the CST \
                 stage, obtained after preprocessing and parsing." in
  Clic.command
    ~group:print_group
    ~desc
    Clic.(args2 syntax display_format)
    Clic.(prefixes ["print";"cst"] @@ source_file @@ stop)
    f

let print_ast =
  let f (syntax, display_format) source_file () =
    return_result@@
    Api.Print.ast source_file syntax display_format
  in
  let _cmdname = "print ast" in
  let _doc = "Subcommand: Print the AST.\n Warning: Intended for development of LIGO and can break at any time." in
  let desc =      "This sub-command prints the source file in the AST \
                 imperative stage, before sugaring step is applied." in
  Clic.command
    ~group:print_group
    ~desc
    Clic.(args2 syntax display_format)
    Clic.(prefixes ["print";"ast"] @@ source_file @@ stop)
    f


let print_ast_sugar =
  let f (syntax, display_format) source_file () =
    return_result @@
    Api.Print.ast_sugar source_file syntax display_format
  in
  let _cmdname = "print ast-sugar" in
  let _doc = "Subcommand: Print the AST.\n Warning: Intended for development of LIGO and can break at any time." in
  let desc =     "This sub-command prints the source file in the AST \
                 stage, after sugaring step is applied." in
  Clic.command
    ~group:print_group
    ~desc
    Clic.(args2 syntax display_format)
    Clic.(prefixes ["print";"ast-sugar"] @@ source_file @@ stop)
    f

let print_ast_core =
  let f (syntax, infer, protocol_version, display_format) source_file () =
    return_result @@
    Api.Print.ast_core source_file syntax infer protocol_version display_format
  in
  let _doc = "Subcommand: Print the AST.\n Warning: Intended for development of LIGO and can break at any time." in
  let desc =      "This sub-command prints the source file in the AST \
                 core stage." in
  Clic.command
    ~group:print_group
    ~desc
    Clic.(args4 syntax infer protocol_version display_format)
    Clic.(prefixes ["print";"ast-core"] @@ source_file @@ stop)
    f

let print_ast_typed =
  let f (syntax, infer, protocol_version, display_format) source_file () =
    return_result @@
    Api.Print.ast_typed source_file syntax infer protocol_version display_format
  in
  let _doc = "Subcommand: Print the typed AST.\n Warning: Intended for development of LIGO and can break at any time." in
  let desc =    "This sub-command prints the source file in the AST \
                 typed stage. Internally, it uses the build system to \
                 type the contract, but the contract is not combined \
                 with imported modules." in
  Clic.command
    ~group:print_group
    ~desc
    Clic.(args4 syntax infer protocol_version display_format)
    Clic.(prefixes ["print";"ast-typed"] @@ source_file @@ stop)
    f

let print_ast_combined =
  let f (syntax, infer, protocol_version, display_format) source_file () =
    return_result @@
    Api.Print.ast_combined source_file syntax infer protocol_version display_format
  in
  let _cmdname = "print ast-combined" in
  let _doc = "Subcommand: Print the contract after combination with the build system.\n Warning: Intended for development of LIGO and can break at any time." in
  let desc =     "This sub-command prints the source file in the AST \
                 typed stage. Internally, it uses the build system to \
                 type the contract, and the contract is combined with \
                 the imported modules." in
  Clic.command
    ~group:print_group
    ~desc
    Clic.(args4 syntax infer protocol_version display_format)
    Clic.(prefixes ["print";"ast-combined"] @@ source_file @@ stop)
    f

let print_mini_c =
  let f (syntax, infer, protocol_version, display_format, optimize) source_file () =
    return_result @@
    Api.Print.mini_c source_file syntax infer protocol_version display_format optimize
  in
  let _cmdname = "print-mini-c" in
  let _doc = "Subcommand: Print Mini-C. Warning: Intended for development of LIGO and can break at any time." in
  let desc =    "This sub-command prints the source file in the Mini-C \
                 stage. Internally, it uses the build system to type \
                 and compile the contract. Compilation is applied \
                 after combination in the AST typed stage." in
  Clic.command
    ~group:print_group
    ~desc
    Clic.(args5 syntax infer protocol_version display_format optimize)
    Clic.(prefixes ["print";"mini-c"] @@ source_file @@ stop)
    f


let dump_changelog =
  let f display_format () =
    return_result @@ Api.dump_changelog display_format in
  let cmdname = "changelog" in
  let doc = "Dump the LIGO changelog to stdout." in
  Clic.command
    ~desc:doc
    Clic.(args1 display_format)
    Clic.(prefix cmdname stop)
    f

let repl =
  let f (protocol_version, infer, amount, balance, sender, source, now, display_format, init_file) syntax_name () =
    return_result @@
    (let protocol = Environment.Protocols.protocols_to_variant protocol_version in
    let syntax = Ligo_compile.Helpers.syntax_to_variant (Syntax_name syntax_name) None in
    let dry_run_opts = Ligo_run.Of_michelson.make_dry_run_options {now ; amount ; balance ; sender ; source ; parameter_ty = None } in
    match protocol, Trace.to_option syntax, Trace.to_option dry_run_opts with
    | _, None, _ -> Error ("", "Please check syntax name.")
    | None, _, _ -> Error ("", "Please check protocol name.")
    | _, _, None -> Error ("", "Please check run options.")
    | Some protocol, Some syntax, Some dry_run_opts ->
       (Repl.main syntax display_format protocol infer dry_run_opts init_file); Ok("","")) in
  let _doc = "Subcommand: REPL" in
  let desc = "" in
  Clic.command ~desc
    Clic.(args9 protocol_version infer amount balance sender source now display_format init_file)
    Clic.(prefix "repl" @@ req_syntax @@ stop)
    f

let main = [
    compile_file ;
    compile_parameter ;
    compile_storage ;
    compile_expression ;
    transpile_contract ;
    transpile_expression ;
    mutate_cst;
    mutate_ast;
    test ;
    dry_run ;
    evaluate_call ~cmdname_deprecation:`deprecated_run_function ;
    evaluate_call ~cmdname_deprecation:`evaluate_call ;
    evaluate_expr ~cmdname_deprecation:`deprecated_evaluate_value ;
    evaluate_expr ~cmdname_deprecation:`evaluate_expr ;
    interpret ;
    list_declarations ;
    measure_contract ;
    get_scope;
    preprocessed;
    print_graph ;
    pretty_print;
    print_cst ;
    print_ast ;
    print_ast_sugar ;
    print_ast_core ;
    print_ast_typed ;
    print_ast_combined ;
    print_mini_c ;
    repl;
    dump_changelog ;
]




let run ?argv () =
  let open Lwt in
  let executable_name = "ligo" in
  let main_with_man =
      Clic.add_manual
        ~executable_name
        ~global_options
        (if Unix.isatty Unix.stdout then Clic.Ansi else Clic.Plain)
        Format.std_formatter
        main in
  let arg = match Option.map ~f:Array.to_list @@ argv with
  | Some(_ :: argv) -> argv
  | _ -> [] in
  let run () =
    (* When fixing tezos-clic, only use dispatch *)
    let version_flag = Clic.args1 @@ Clic.switch ~doc:"" ~long:"version" () in
    Clic.parse_global_options version_flag () arg >>= function
      Ok (v,argv) ->
        if v then (Format.printf "%s\n" Version.version;Lwt.return_ok ())
        else
          Clic.dispatch main_with_man () @@ argv
    | Error err -> Lwt.return_error err
  in
  (Lwt_main.run
  (Lwt.catch (fun () -> run ()
      >>= function
      | Ok () -> Lwt.return 0
      | Error [Clic.Help command] ->
        Clic.usage
          Format.std_formatter
          ~executable_name
          ~global_options
          (match command with None -> [] | Some c -> [c]);
          Lwt.return 0
      (* Doesn't work at the moment *)
      | Error [Clic.Version] ->
          let version = Version.version in
          Format.printf "%s\n" version ;
          Lwt.return 0
      | Error [Proto_alpha_utils.Error_monad.Exn Cli_helpers.Error_compilation] ->
        Lwt.return 1
      | Error errs ->
          match arg with
          (* remove weird auto-complete bug from old cli*)
           "run"::_ ->
            Clic.pp_cli_errors
                Format.err_formatter
                ~executable_name
                ~global_options
                ~default:Proto_alpha_utils.Error_monad.pp
                errs; Lwt.return 0
          | _ ->
          let result = Old_cli.run ?argv () in
          match result with
            `Ok () ->
              if List.mem ~equal:String.equal arg "--format=json" then () else
              Format.eprintf "Warning: The old cli is deprecated, use `ligo --help` or `ligo man` to consult the new command syntax\n";
              Lwt.return 0
          | _ -> (
            let buffer = Buffer.contents Old_cli.buffer in
            if buffer = "ligo: \n" || buffer = "" then
              Format.eprintf "Warning: The old cli is deprecated, use `ligo --help` or `ligo man` to consult the new command syntax\n"
            else
            Clic.pp_cli_errors
                Format.err_formatter
                ~executable_name
                ~global_options
                ~default:Proto_alpha_utils.Error_monad.pp
                errs
              ;
          (* This is to compensate that ligo compile contract --help throw an error (missing positional argument)  *)
            Lwt.return 0)
  >>= fun retcode ->
      Format.pp_print_flush Format.err_formatter () ;
      Format.pp_print_flush Format.std_formatter () ;
      Lwt.return retcode)
     (fun v ->
       let message msg = Format.eprintf "An internal error ocurred. Please, contact the developers.@.";
                         if !is_dev then
                           Format.eprintf "%s.@." msg;
                         Format.pp_print_flush Format.err_formatter () ;
                         Lwt.return 1 in
       match v with
       | Failure msg -> message msg
       | exn -> message (Printexc.to_string exn))))
