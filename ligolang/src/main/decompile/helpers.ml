open Trace
open Main_errors

type s_syntax = Syntax_name of string
type s_dialect = Dialect_name of string

type v_syntax =
  | PascaLIGO of Tree_abstraction.Pascaligo.Decompiler.dialect option
  | CameLIGO
  | ReasonLIGO
  | JsLIGO

let dialect_to_variant ~raise dialect =
  match dialect with
  | None -> None
  | Some (Dialect_name dialect) ->
     match dialect with
     | "terse" -> (Some Tree_abstraction.Pascaligo.Decompiler.Terse)
     | "verbose" -> (Some Tree_abstraction.Pascaligo.Decompiler.Verbose)
     | _ -> raise.raise (`Main_invalid_dialect_name dialect)

let syntax_to_variant ~raise ?dialect (Syntax_name syntax) source =
  match syntax, source with
    "auto", Some sf ->
      (match Filename.extension sf with
         ".ligo" | ".pligo" ->
                    let dialect = dialect_to_variant ~raise dialect in
                    (PascaLIGO dialect)
       | ".mligo"           -> CameLIGO
       | ".religo"          -> ReasonLIGO
       | ".jsligo"          -> JsLIGO
       | ext                -> raise.raise (syntax_auto_detection ext))
  | ("pascaligo" | "PascaLIGO"),   _ ->
     let dialect = dialect_to_variant ~raise dialect in
     (PascaLIGO dialect)
  | ("cameligo" | "CameLIGO"),     _ -> CameLIGO
  | ("reasonligo" | "ReasonLIGO"), _ -> ReasonLIGO
  | ("jsligo" | "JsLIGO"),         _ -> JsLIGO
  | _ -> raise.raise (invalid_syntax syntax)

let specialise_and_print_pascaligo dialect m =
  let ast = Self_ast_imperative.decompile_imperative m in
  let cst = Tree_abstraction.Pascaligo.decompile_module ?dialect ast in
  let source = (Parsing.Pascaligo.pretty_print cst)
  in source

let specialise_and_print_expression_pascaligo dialect expression =
  let ast = Self_ast_imperative.decompile_imperative_expression expression in
  let cst = Tree_abstraction.Pascaligo.decompile_expression ?dialect ast in
  let source =(Parsing.Pascaligo.pretty_print_expression cst)
  in source

let specialise_and_print_cameligo m =
  let cst = Tree_abstraction.Cameligo.decompile_module m in
  let source = (Parsing.Cameligo.pretty_print cst)
  in source

let specialise_and_print_expression_cameligo expression =
  let cst = Tree_abstraction.Cameligo.decompile_expression expression in
  let source = (Parsing.Cameligo.pretty_print_expression cst)
  in source

let specialise_and_print_reasonligo m =
  let cst = Tree_abstraction.Reasonligo.decompile_module m in
  let source = (Parsing.Reasonligo.pretty_print cst)
  in source

let specialise_and_print_expression_reasonligo expression =
  let cst =
    Tree_abstraction.Reasonligo.decompile_expression expression in
  let source =
    (Parsing.Reasonligo.pretty_print_expression cst)
  in source

let specialise_and_print_jsligo m =
  let ast =
    Self_ast_imperative.decompile_imperative m in
  let cst =
    Tree_abstraction.Jsligo.decompile_module ast in
  let source =
    (Parsing.Jsligo.pretty_print cst)
  in source

let specialise_and_print_expression_jsligo expression =
  let ast =
    Self_ast_imperative.decompile_imperative_expression expression in
  let cst =
    Tree_abstraction.Jsligo.decompile_expression ast in
  let b = Buffer.create 100 in
  List.fold ~f:(fun all x -> 
    let source =
    (Parsing.Jsligo.pretty_print_expression x) in
    Buffer.add_buffer all source; 
    b
  ) ~init:b cst


let specialise_and_print syntax source : Buffer.t =
  let specialise_and_print =
    match syntax with
      PascaLIGO dialect -> specialise_and_print_pascaligo dialect
    | CameLIGO   -> specialise_and_print_cameligo
    | ReasonLIGO -> specialise_and_print_reasonligo
    | JsLIGO     -> specialise_and_print_jsligo in
  specialise_and_print source

let specialise_and_print_expression syntax source =
  let specialise_and_print = match syntax with
    PascaLIGO dialect -> specialise_and_print_expression_pascaligo dialect
  | CameLIGO   -> specialise_and_print_expression_cameligo
  | ReasonLIGO -> specialise_and_print_expression_reasonligo
  | JsLIGO     -> specialise_and_print_expression_jsligo in
  specialise_and_print source
