open Cmdliner
open Old_cli_helpers

let version = Version.version

let main =
  let man =
    [ `S "MORE HELP";
      `P "Use `$(mname) $(i,COMMAND) --help' for help on a single command.";
      `S "DOCUMENTATION";
      `P "https://ligolang.org/docs/intro/introduction";
      `S "ASK A QUESTION";
      `P "https://discord.gg/9rhYaEt";
      `S "OPEN AN ISSUE";
      `P "https://gitlab.com/ligolang/ligo/issues/new"
      ]
    in
    (Term.(ret (const (`Help (`Groff, None)))), Term.info "ligo" ~version ~man)

let source_file n =
  let open Arg in
  let info =
    let docv = "SOURCE_FILE" in
    let doc = "$(docv) is the path to the smart contract file." in
    info ~docv ~doc [] in
  required @@ pos n (some non_dir_file) None info

let entry_point n =
  let open Arg in
  let info =
    let docv = "ENTRY_POINT" in
    let doc = "$(docv) is entry-point that will be compiled." in
    info ~docv ~doc [] in
  required @@ pos n (some string) (Some "main") info

let on_chain_views =
  let open Arg in
  let info =
    let docv = "ON_CHAIN_VIEWS" in
    let doc = "$(docv) is a declaration name list that will be compiled as on-chain views" in
    info ~docv ~doc ["views" ; "v"] in
  value @@ opt (list string) [] info

let expression purpose n =
  let open Arg in
  let docv = purpose ^ "_EXPRESSION" in
  let doc = "$(docv) is the expression that will be compiled." in
  let info = info ~docv ~doc [] in
  required @@ pos n (some string) None info

let libraries =
  let open Arg in
  let docv = "LIBRARY" in
  let doc = "$(docv) is a path to a directory containing included files" in
  let info = info ~docv ~doc ["lib" ; "l"] in
  value @@ opt_all string [] info

let syntax =
  let open Arg in
  let info =
    let docv = "SYNTAX" in
    let doc = "$(docv) is the syntax that will be used. Currently supported syntaxes are \"pascaligo\", \"cameligo\", \"reasonligo\" and \"jsligo\". By default, the syntax is guessed from the extension (.ligo, .mligo, .religo, and .jsligo respectively)." in
    info ~docv ~doc ["syntax" ; "s"] in
  value @@ opt string "auto" info

let steps =
  let open Arg in
  let info =
    let docv = "STEPS" in
    let doc = "$(docv) is a bound in the number of steps to be done by the interpreter." in
    info ~docv ~doc ["steps" ; "n"] in
  value @@ opt string "1000000" info

let protocol_version =
  let open Arg in
  let open Environment.Protocols in
  let plist = Format.asprintf "%a" (Simple_utils.PP_helpers.list_sep_d_par Format.pp_print_string) protocols_str in
  let info =
    let docv = "PROTOCOL_VERSION" in
    let doc = Format.asprintf "$(docv) will decide protocol's types/values pre-loaded into the LIGO environment %s. \
                               By default, the current protocol (%s) will be used" plist (variant_to_string current) in
    info ~docv ~doc ["protocol" ; "p"] in
  value @@ opt string "current" info

let dialect =
  let open Arg in
  let info =
    let docv = "PASCALIGO_DIALECT" in
    let doc = "$(docv) is the pascaligo dialect that will be used. Currently supported dialects are \"terse\" and \"verbose\". By default the dialect is \"terse\"." in
    info ~docv ~doc ["dialect" ; "d"] in
  value @@ opt string "terse" info

let req_syntax n =
  let open Arg in
  let info =
    let docv = "SYNTAX" in
    let doc = "$(docv) is the syntax that will be used. Currently supported syntaxes are \"pascaligo\", \"cameligo\" and \"reasonligo\". By default, the syntax is guessed from the extension (.ligo, .mligo, .religo, .jsligo respectively)." in
    info ~docv ~doc [] in
  required @@ pos n (some string) None info

let init_file =
  let open Arg in
  let info =
    let docv = "INIT_FILE" in
    let doc = "$(docv) is the path to smart contract file to be used for context initialization." in
    info ~docv ~doc ["init-file"] in
  value @@ opt (some string) None info

let amount =
  let open Arg in
  let info =
    let docv = "AMOUNT" in
    let doc = "$(docv) is the amount the Michelson interpreter will use for the transaction." in
    info ~docv ~doc ["amount"] in
  value @@ opt string "0" info

let balance =
  let open Arg in
  let info =
    let docv = "BALANCE" in
    let doc = "$(docv) is the balance the Michelson interpreter will use for the contract balance." in
    info ~docv ~doc ["balance"] in
  value @@ opt string "0" info

let sender =
  let open Arg in
  let info =
    let docv = "SENDER" in
    let doc = "$(docv) is the sender the Michelson interpreter transaction will use." in
    info ~docv ~doc ["sender"] in
  value @@ opt (some string) None info

let source =
  let open Arg in
  let info =
    let docv = "SOURCE" in
    let doc = "$(docv) is the source the Michelson interpreter transaction will use." in
    info ~docv ~doc ["source"] in
  value @@ opt (some string) None info

let disable_michelson_typechecking =
  let open Arg in
  let info =
    let doc = "disable Michelson typecking, this might produce ill-typed Michelson code." in
    info ~doc ["disable-michelson-typechecking"] in
  value @@ flag info

let without_run =
  let open Arg in
  let info =
    let doc = "disable running of compiled expression." in
    info ~doc ["without-run"] in
  value @@ flag info

let with_types =
  let open Arg in
  let info =
    let doc = "tries to infer types for all named expressions" in
    info ~doc ["with-types"] in
  value @@ flag info

let now =
  let open Arg in
  let info =
    let docv = "NOW" in
    let doc = "$(docv) is the NOW value the Michelson interpreter will use (e.g. '2000-01-01T10:10:10Z')" in
    info ~docv ~doc ["now"] in
  value @@ opt (some string) None info

let display_format =
  let open Arg in
  let open Display in
  let info  =
    let docv = "DISPLAY_FORMAT" in
    let doc = "$(docv) is the format that will be used by the CLI. Available formats are 'dev', 'json', and 'human-readable' (default). When human-readable lacks details (we are still tweaking it), please contact us and use another format in the meanwhile." in
    info ~docv ~doc ["format" ; "display-format"] in
  value @@
  opt
    (enum [("human-readable", human_readable); ("dev", dev); ("json", json)])
    human_readable
    info

let output_file =
  let open Arg in
  let info  =
    let docv = "OUTPUT_FILE" in
    let doc = "$(docv) if used, prints the output into the specified file instead of stdout" in
    info ~docv ~doc ["output" ; "output-file"] in
  value @@ opt (some string) None info

let michelson_code_format =
  let open Arg in
  let info  =
    let docv = "MICHELSON_FORMAT" in
    let doc = "$(docv) is the format that will be used by compile-contract for the resulting Michelson. Available formats are 'text' (default), 'json' and 'hex'." in
    info ~docv ~doc ["michelson-format"] in
  value @@
  opt
    (enum [("text", `Text); ("json", `Json); ("hex", `Hex)])
    `Text info

let optimize =
  let open Arg in
  let docv = "OPTIMIZE" in
  let doc = "Apply Mini-C optimizations as if compiling $(docv)" in
  let info =
    info ~docv ~doc ["optimize"] in
  value @@ opt (some string) None info

let infer =
  let open Arg in
  let info =
    let doc = "enable type inference" in
    info ~doc ["infer"] in
    value @@ flag info

let warn =
  let open Arg in
  let info =
    let docv = "BOOL" in
    let doc = "$(docv) indicates whether warning messages should be printed in stderr or not" in
    info ~docv ~doc ["warn"] in
    value @@ opt bool true info

let werror =
  let open Arg in
  let info =
    let docv = "BOOL" in
    let doc = "$(docv) indicates whether warning messages should be treated as errors or not" in
    info ~docv ~doc ["werror"] in
  value @@ opt bool false info

let seed =
  let open Arg in
  let info =
    let docv = "SEED" in
    let doc = "$(docv) is the seed or counter used for generation." in
    info ~docv ~doc ["seed"] in
  value @@ opt (some int) None info

let generator =
  let open Arg in
  let info =
    let docv = "GENERATOR" in
    let doc = "$(docv) is the generator for mutation." in
    info ~docv ~doc ["generator" ; "g"] in
  value @@ opt string "random" info

module Api = Ligo_api
let compile_file =
  let f source_file entry_point oc_views syntax infer protocol_version display_format disable_typecheck michelson_format output_file warn werror =
    return_result ~warn ?output_file @@
    Api.Compile.contract ~werror source_file entry_point oc_views syntax infer protocol_version display_format disable_typecheck michelson_format in
  let term = Term.(const f $ source_file 0 $ entry_point 1 $ on_chain_views $ syntax $ infer $ protocol_version $ display_format $ disable_michelson_typechecking $ michelson_code_format $ output_file $ warn $ werror) in
  let cmdname = "compile-contract" in
  let doc = "Subcommand: Compile a contract." in
  let man = [`S Manpage.s_description;
             `P "This sub-command compiles a contract to Michelson \
                 code. It expects a source file and an entrypoint \
                 function that has the type of a contract: \"parameter \
                 * storage -> operations list * storage\"."]
  in (Term.ret term , Term.info ~man ~doc cmdname)

let preprocess =
  let f source_file syntax display_format =
    return_result @@
      Api.Print.preprocess source_file syntax display_format in
  let term = Term.(const f $ source_file 0 $ syntax $ display_format) in
  let cmdname = "preprocess" in
  let doc = "Subcommand: Preprocess the source file.\nWarning: Intended for development of LIGO and can break at any time." in
  let man = [`S Manpage.s_description;
             `P "This sub-command runs the pre-processor on a LIGO \
                 source file and outputs the result. The directive \
                 `#include` directly inlines the included file and \
                 therefore its content appears in the output. In \
                 contrast, the directive `#import` includes the file \
                 as a module and therefore the content of the imported \
                 file is not printed by this sub-command."]
  in (Term.ret term, Term.info ~man ~doc cmdname)

let pretty_print =
  let f source_file syntax display_format =
    return_result @@ 
    Api.Print.pretty_print source_file syntax display_format in
  let term = Term.(const f $ source_file 0 $ syntax $ display_format) in
  let cmdname = "pretty-print" in
  let doc = "Subcommand: Pretty-print the source file." in
  let man = [`S Manpage.s_description;
             `P "This sub-command pretty-prints a source file in \
                 LIGO. The width of the pretty-printed text is \
                 adjusted to the number of columns in the terminal (or \
                 60 if it cannot be determined)."]
  in (Term.ret term, Term.info ~man ~doc cmdname)

let print_graph =
  let f source_file syntax display_format =
    return_result @@
    Api.Print.dependency_graph source_file syntax display_format
  in
  let term = Term.(const f $ source_file 0  $ syntax $ display_format) in
  let cmdname = "print-graph" in
  let doc = "Subcommand: Print the dependency graph.\nWarning: Intended for development of LIGO and can break at any time." in
  let man = [`S Manpage.s_description;
             `P "This sub-command prints the dependency graph created \
                 by the module system. It explores all imported source \
                 files (recursively) following a DFS strategy."]
  in (Term.ret term, Term.info ~man ~doc cmdname)

let print_cst =
  let f source_file syntax display_format =
    return_result @@
    Api.Print.cst source_file syntax display_format
  in
  let term = Term.(const f $ source_file 0  $ syntax $ display_format) in
  let cmdname = "print-cst" in
  let doc = "Subcommand: Print the CST.\nWarning: Intended for development of LIGO and can break at any time." in
  let man = [`S Manpage.s_description;
             `P "This sub-command prints the source file in the CST \
                 stage, obtained after preprocessing and parsing."]
  in (Term.ret term, Term.info ~man ~doc cmdname)

let print_ast =
  let f source_file syntax display_format =
    return_result@@
    Api.Print.ast source_file syntax display_format
  in
  let term = Term.(const f $ source_file 0 $ syntax $ display_format) in
  let cmdname = "print-ast" in
  let doc = "Subcommand: Print the AST.\n Warning: Intended for development of LIGO and can break at any time." in
  let man = [`S Manpage.s_description;
             `P "This sub-command prints the source file in the AST \
                 imperative stage, before sugaring step is applied."]
  in (Term.ret term, Term.info ~man ~doc cmdname)


let print_ast_sugar =
  let f source_file syntax display_format =
    return_result @@
    Api.Print.ast_sugar source_file syntax display_format
  in
  let term = Term.(const f $ source_file 0  $ syntax $ display_format) in
  let cmdname = "print-ast-sugar" in
  let doc = "Subcommand: Print the AST.\n Warning: Intended for development of LIGO and can break at any time." in
  let man = [`S Manpage.s_description;
             `P "This sub-command prints the source file in the AST \
                 stage, after sugaring step is applied."]
  in (Term.ret term, Term.info ~man ~doc cmdname)

let print_ast_core =
  let f source_file syntax infer protocol_version display_format =
    return_result @@
    Api.Print.ast_core source_file syntax infer protocol_version display_format
  in
  let term = Term.(const f $ source_file 0  $ syntax $ infer $ protocol_version $ display_format) in
  let cmdname = "print-ast-core" in
  let doc = "Subcommand: Print the AST.\n Warning: Intended for development of LIGO and can break at any time." in
  let man = [`S Manpage.s_description;
             `P "This sub-command prints the source file in the AST \
                 core stage."]
  in (Term.ret term, Term.info ~man ~doc cmdname)

let print_ast_typed =
  let f source_file syntax infer protocol_version display_format =
    return_result @@
    Api.Print.ast_typed source_file syntax infer protocol_version display_format
  in
  let term = Term.(const f $ source_file 0  $ syntax $ infer $ protocol_version $ display_format) in
  let cmdname = "print-ast-typed" in
  let doc = "Subcommand: Print the typed AST.\n Warning: Intended for development of LIGO and can break at any time." in
  let man = [`S Manpage.s_description;
             `P "This sub-command prints the source file in the AST \
                 typed stage. Internally, it uses the build system to \
                 type the contract, but the contract is not combined \
                 with imported modules."]
  in (Term.ret term, Term.info ~man ~doc cmdname)

let print_ast_combined =
  let f source_file syntax infer protocol_version display_format =
    return_result @@
    Api.Print.ast_combined source_file syntax infer protocol_version display_format
  in
  let term = Term.(const f $ source_file 0  $ syntax $ infer $ protocol_version $ display_format) in
  let cmdname = "print-ast-combined" in
  let doc = "Subcommand: Print the contract after combination with the build system.\n Warning: Intended for development of LIGO and can break at any time." in
  let man = [`S Manpage.s_description;
             `P "This sub-command prints the source file in the AST \
                 typed stage. Internally, it uses the build system to \
                 type the contract, and the contract is combined with \
                 the imported modules."]
  in (Term.ret term, Term.info ~man ~doc cmdname)

let print_mini_c =
  let f source_file syntax infer protocol_version display_format optimize =
    return_result @@
    Api.Print.mini_c source_file syntax infer protocol_version display_format optimize
  in
  let term = Term.(const f $ source_file 0 $ syntax $ infer $ protocol_version $ display_format $ optimize) in
  let cmdname = "print-mini-c" in
  let doc = "Subcommand: Print Mini-C. Warning: Intended for development of LIGO and can break at any time." in
  let man = [`S Manpage.s_description;
             `P "This sub-command prints the source file in the Mini-C \
                 stage. Internally, it uses the build system to type \
                 and compile the contract. Compilation is applied \
                 after combination in the AST typed stage."]
  in (Term.ret term, Term.info ~man ~doc cmdname)

let measure_contract =
  let f source_file entry_point oc_views syntax infer protocol_version display_format warn werror =
    return_result ~warn @@
    Api.Info.measure_contract source_file entry_point oc_views syntax infer protocol_version display_format werror
  in
  let term =
    Term.(const f $ source_file 0 $ entry_point 1 $ on_chain_views $ syntax $ infer $ protocol_version $ display_format $ warn $ werror) in
  let cmdname = "measure-contract" in
  let doc = "Subcommand: Measure a contract's compiled size in bytes." in
  let man = [`S Manpage.s_description;
             `P "This sub-command compiles a source file and measures \
                 the contract's compiled size in bytes."]
  in (Term.ret term , Term.info ~man ~doc cmdname)

let compile_parameter =
  let f source_file entry_point expression syntax infer protocol_version amount balance sender source now display_format michelson_format output_file warn werror =
    return_result ~warn ?output_file @@
    Api.Compile.parameter source_file entry_point expression syntax infer protocol_version amount balance sender source now display_format michelson_format werror
    in
  let term =
    Term.(const f $ source_file 0 $ entry_point 1 $ expression "PARAMETER" 2  $ syntax $ infer $ protocol_version $ amount $ balance $ sender $ source $ now $ display_format $ michelson_code_format $ output_file $ warn $ werror) in
  let cmdname = "compile-parameter" in
  let doc = "Subcommand: Compile parameters to a Michelson expression." in
  let man = [`S Manpage.s_description;
             `P "This sub-command compiles a parameter for a given \
                 contract to a Michelson expression. The resulting \
                 Michelson expression can be passed as an argument in \
                 a transaction which calls a contract."]
  in (Term.ret term , Term.info ~man ~doc cmdname)

let interpret =
  let f expression init_file syntax infer protocol_version amount balance sender source now display_format =
    return_result @@
    Api.Run.interpret expression init_file syntax infer protocol_version amount balance sender source now display_format
  in
  let term =
    Term.(const f $ expression "EXPRESSION" 0 $ init_file $ syntax $ infer $ protocol_version $ amount $ balance $ sender $ source $ now $ display_format) in
  let cmdname = "interpret" in
  let doc = "Subcommand: Interpret the expression in the context initialized by the provided source file." in
  let man = [`S Manpage.s_description;
             `P "This sub-command interprets a LIGO expression. The \
                 context can be initialized by providing a source \
                 file. The interpretation is done using Michelson's \
                 interpreter."]
  in (Term.ret term , Term.info ~man ~doc cmdname)

let compile_storage =
  let f source_file entry_point expression syntax infer protocol_version amount balance sender source now display_format michelson_format output_file warn werror =
    return_result ~warn ?output_file @@
    Api.Compile.storage source_file entry_point expression syntax infer protocol_version amount balance sender source now display_format michelson_format werror
  in
  let term =
    Term.(const f $ source_file 0 $ entry_point 1 $ expression "STORAGE" 2  $ syntax $ infer $ protocol_version $ amount $ balance $ sender $ source $ now $ display_format $ michelson_code_format $ output_file $ warn $ werror) in
  let cmdname = "compile-storage" in
  let doc = "Subcommand: Compile an initial storage in LIGO syntax to \
             a Michelson expression." in
  let man = [`S Manpage.s_description;
             `P "This sub-command compiles an initial storage for a \
                 given contract to a Michelson expression. The \
                 resulting Michelson expression can be passed as an \
                 argument in a transaction which originates a contract."]
  in (Term.ret term , Term.info ~man ~doc cmdname)

let dry_run =
  let f source_file entry_point input storage amount balance sender source now syntax infer protocol_version display_format warn werror =
    return_result ~warn @@
    Api.Run.dry_run source_file entry_point input storage amount balance sender source now syntax infer protocol_version display_format werror
    in
  let term =
    Term.(const f $ source_file 0 $ entry_point 1 $ expression "PARAMETER" 2 $ expression "STORAGE" 3 $ amount $ balance $ sender $ source $ now  $ syntax $ infer $ protocol_version $ display_format $ warn $ werror) in
  let cmdname = "dry-run" in
  let doc = "Subcommand: Run a smart-contract with the given storage and input." in
  let man = [`S Manpage.s_description;
             `P "This sub-command runs a LIGO contract on a given \
                 storage and parameter. The context is initialized \
                 from a source file where the contract is \
                 implemented. The interpretation is done using \
                 Michelson's interpreter."]
  in (Term.ret term , Term.info ~man ~doc cmdname)

let evaluate_call ~cmdname_deprecation =
  let f source_file entry_point parameter amount balance sender source now syntax infer protocol_version display_format warn werror =
    return_result ~warn @@
    Api.Run.evaluate_call source_file entry_point parameter amount balance sender source now syntax infer protocol_version display_format werror
    in
  let term =
    Term.(const f $ source_file 0 $ entry_point 1 $ expression "PARAMETER" 2 $ amount $ balance $ sender $ source $ now  $ syntax $ infer $ protocol_version $ display_format $ warn $ werror) in
  (* "run-function" was renamed to "evaluate-call", keeping both for a few versions for backward-compatibility. *)
  let cmdname = match cmdname_deprecation with
  | `deprecated_run_function -> "run-function"
  | `evaluate_call -> "evaluate-call" in
  let deprecation = match cmdname_deprecation with
  | `deprecated_run_function -> "Deprecated, renamed to evaluate-call. Use evaluate-call instead. "
  | `evaluate_call -> "" in
  let doc = deprecation ^ "Subcommand: Run a function with the given parameter." in
  let man = [`S Manpage.s_description;
             `P (deprecation ^
                 "This sub-command runs a LIGO function on a given \
                  argument. The context is initialized from a source \
                  file where the function is implemented. The \
                  interpretation is done using Michelson's interpreter.")]
  in (Term.ret term , Term.info ~man ~doc cmdname)

let evaluate_expr ~cmdname_deprecation =
  let f source_file entry_point amount balance sender source now syntax infer protocol_version display_format warn werror =
    return_result ~warn @@
    Api.Run.evaluate_expr source_file entry_point amount balance sender source now syntax infer protocol_version display_format werror
    in
  let term =
    Term.(const f $ source_file 0 $ entry_point 1 $ amount $ balance $ sender $ source $ now  $ syntax $ infer $ protocol_version $ display_format $ warn $ werror) in
  (* "run-function" was renamed to "evaluate-call", keeping both for a few versions for backward-compatibility. *)
  let cmdname = match cmdname_deprecation with
  | `deprecated_evaluate_value -> "evaluate-value"
  | `evaluate_expr -> "evaluate-expr" in
  let deprecation = match cmdname_deprecation with
  | `deprecated_evaluate_value -> "Deprecated, renamed to evaluate-expr. Use evaluate-expr instead. "
  | `evaluate_expr -> "" in
  let doc = deprecation ^ "Subcommand: Evaluate a given definition." in
  let man = [`S Manpage.s_description;
             `P (deprecation ^
                 "This sub-command evaluates a LIGO definition. The \
                  context is initialized from a source file where the \
                  definition is written. The interpretation is done \
                  using a Michelson interpreter.")]
  in (Term.ret term , Term.info ~man ~doc cmdname)

let compile_expression =
  let f expression syntax infer protocol_version init_file display_format without_run michelson_format warn werror =
    return_result ~warn @@
    Api.Compile.expression expression syntax infer protocol_version init_file display_format without_run michelson_format werror
    in
  let term =
    Term.(const f $ expression "" 1 $ req_syntax 0 $ infer $ protocol_version $ init_file $ display_format $ without_run $ michelson_code_format $ warn $ werror) in
  let cmdname = "compile-expression" in
  let doc = "Subcommand: Compile to a Michelson value." in
  let man = [`S Manpage.s_description;
             `P "This sub-command compiles a LIGO expression to a \
                 Michelson value. It works by compiling the LIGO \
                 expression to a Michelson expression and then \
                 interpreting it using Michelson's interpreter."]
  in (Term.ret term , Term.info ~man ~doc cmdname)

let dump_changelog =
  let f display_format =
    return_result @@ Api.dump_changelog display_format in
  let term =
    Term.(const f $ display_format) in
  let cmdname = "changelog" in
  let doc = "Dump the LIGO changelog to stdout." in
  let man = [`S Manpage.s_description;
             `P "This sub-command dumps the changelog to the stdout."]
  in (Term.ret term , Term.info ~man ~doc cmdname)

let list_declarations =
  let f source_file syntax display_format =
    return_result @@
    Api.Info.list_declarations source_file syntax display_format
  in
  let term =
    Term.(const f $ source_file 0  $ syntax $ display_format) in
  let cmdname = "list-declarations" in
  let doc = "Subcommand: List all the top-level declarations." in
  let man = [`S Manpage.s_description;
             `P "This sub-command prints a list of all top-level \
                 declarations (not including types and modules)."]
  in (Term.ret term , Term.info ~man ~doc cmdname)

let transpile_contract =
  let f source_file new_syntax syntax new_dialect display_format output_file =
    return_result ?output_file @@
    Api.Transpile.contract source_file new_syntax syntax new_dialect display_format
  in
  let term =
    Term.(const f $ source_file 0 $ req_syntax 1  $ syntax $ dialect $ display_format $ output_file) in
  let cmdname = "transpile-contract" in
  let doc = "Subcommand: Transpile a contract to another syntax (BETA)." in
  let man = [`S Manpage.s_description;
             `P "This sub-command transpiles a source file to another \
                 syntax. It does not use the build system, but the \
                 source file is preprocessed. Comments are currently \
                 not transpiled. Please use at your own risk."]
  in (Term.ret term , Term.info ~man ~doc cmdname)

let transpile_expression =
  let f expression new_syntax syntax new_dialect display_format =
    return_result @@
    Api.Transpile.expression expression new_syntax syntax new_dialect display_format
  in
  let term =
    Term.(const f $ expression "" 1  $ req_syntax 2 $ req_syntax 0 $ dialect $ display_format) in
  let cmdname = "transpile-expression" in
  let doc = "Subcommand: Transpile an expression to another syntax (BETA)." in
  let man = [`S Manpage.s_description;
             `P "This sub-command transpiles a LIGO expression to \
                 another syntax. Comments are currently not \
                 transpiled. Please use at your own risk."]
  in (Term.ret term, Term.info ~man ~doc cmdname)


let get_scope =
  let f source_file syntax infer protocol_version libs display_format with_types =
    return_result @@
    Api.Info.get_scope source_file syntax infer protocol_version libs display_format with_types
  in
  let term =
    Term.(const f $ source_file 0 $ syntax $ infer $ protocol_version $ libraries $ display_format $ with_types) in
  let cmdname = "get-scope" in
  let doc = "Subcommand: Return the JSON encoded environment for a given file." in
  let man = [`S Manpage.s_description;
             `P "This sub-command returns the environment for a given \
                 file in JSON format. It does not use the build system."]
  in (Term.ret term , Term.info ~man ~doc cmdname)

let test =
  let f source_file syntax steps infer protocol_version display_format =
    return_result @@
    Api.Run.test source_file syntax steps infer protocol_version display_format
  in
  let term =
    Term.(const f $ source_file 0 $ syntax $ steps $ infer $ protocol_version $ display_format) in
  let cmdname = "test" in
  let doc = "Subcommand: Test a contract with the LIGO test framework (BETA)." in
  let man = [`S Manpage.s_description;
             `P "This sub-command tests a LIGO contract using a LIGO \
                 interpreter. Still \
                 under development, there are features that are work \
                 in progress and are subject to change. No real test \
                 procedure should rely on this sub-command alone.";
            ]
  in (Term.ret term , Term.info ~man ~doc cmdname)

let repl =
  let f syntax_name protocol_version infer
    amount balance sender source now display_format init_file : unit Term.ret =
    (let protocol = Environment.Protocols.protocols_to_variant protocol_version in
    let syntax = Ligo_compile.Helpers.syntax_to_variant (Syntax_name syntax_name) None in
    let dry_run_opts = Ligo_run.Of_michelson.make_dry_run_options {now ; amount ; balance ; sender ; source ; parameter_ty = None } in
    match protocol, Trace.to_option syntax, Trace.to_option dry_run_opts with
    | _, None, _ -> `Error (false, "Please check syntax name.")
    | None, _, _ -> `Error (false, "Please check protocol name.")
    | _, _, None -> `Error (false, "Please check run options.")
    | Some protocol, Some syntax, Some dry_run_opts ->
       `Ok (Repl.main syntax display_format protocol infer dry_run_opts init_file)) in
  let term =
    Term.(const f $ req_syntax 0 $ protocol_version $ infer $ amount $ balance $ sender $ source $ now $ display_format $ init_file) in
  let cmdname = "repl" in
  let doc = "Subcommand: REPL" in
  (Term.ret term , Term.info ~doc cmdname)

let mutate_ast =
  let f source_file syntax infer protocol_version libs display_format seed generator =
    return_result @@
    Api.Mutate.mutate_ast source_file syntax infer protocol_version libs display_format seed generator
  in
  let term =
    Term.(const f $ source_file 0 $ syntax $ infer $ protocol_version $ libraries $ display_format $ seed $ generator) in
  let cmdname = "mutate-ast" in
  let doc = "Subcommand: Return a mutated version for a given file." in
  let man = [`S Manpage.s_description;
             `P "This sub-command returns a mutated version for a \
                 given file. It does not use the build system."]
  in (Term.ret term , Term.info ~man ~doc cmdname)

let mutate_cst =
  let f source_file syntax infer protocol_version libs display_format seed generator =
    return_result @@
    Api.Mutate.mutate_cst source_file syntax infer protocol_version libs display_format seed generator in
  let term =
    Term.(const f $ source_file 0 $ syntax $ infer $ protocol_version $ libraries $ display_format $ seed $ generator) in
  let cmdname = "mutate-cst" in
  let doc = "Subcommand: Return a mutated version for a given file." in
  let man = [`S Manpage.s_description;
             `P "This sub-command returns a mutated version for a \
                 given file. It does not use the build system."]
  in (Term.ret term , Term.info ~man ~doc cmdname)

let buffer = Buffer.create 100

let run ?argv () =
  let err = Format.formatter_of_buffer buffer in
  let result = Term.eval_choice ~err ?argv main [
    test ;
    compile_file ;
    measure_contract ;
    compile_parameter ;
    compile_storage ;
    compile_expression ;
    transpile_contract ;
    transpile_expression ;
    interpret ;
    dry_run ;
    evaluate_call ~cmdname_deprecation:`deprecated_run_function ;
    evaluate_call ~cmdname_deprecation:`evaluate_call ;
    evaluate_expr ~cmdname_deprecation:`deprecated_evaluate_value ;
    evaluate_expr ~cmdname_deprecation:`evaluate_expr ;
    dump_changelog ;
    print_graph ;
    print_cst ;
    print_ast ;
    print_ast_sugar ;
    print_ast_core ;
    print_ast_typed ;
    print_ast_combined ;
    print_mini_c ;
    list_declarations ;
    preprocess;
    pretty_print;
    get_scope;
    repl;
    mutate_ast;
    mutate_cst;
  ] in
  result
