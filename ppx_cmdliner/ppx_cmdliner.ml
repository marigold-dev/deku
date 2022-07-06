open Ppxlib
open Ast_helper

(** Attribute to provide a one line doc to arguments/command **)
let doc_attribute =
  Attribute.declare "cmdliner.doc" Attribute.Context.label_declaration
    Ast_pattern.(single_expr_payload __)
    (fun expr -> expr)

(** Attribute to provide a default value to argument **)
let default_attribute =
  Attribute.declare "cmdliner.default" Attribute.Context.label_declaration
    Ast_pattern.(single_expr_payload __)
    (fun expr -> expr)

(** Attribute to provide an assert function to perform a check on an argument **)
let assert_attribute =
  Attribute.declare "cmdliner.assert" Attribute.Context.label_declaration
    Ast_pattern.(single_expr_payload __)
    (fun expr -> expr)

(** Attribute with no payload to allow passing env variable as argument **)
let env_attribute =
  Attribute.declare "cmdliner.env" Attribute.Context.label_declaration
    Ast_pattern.(pstr nil)
    ()

(** Builds the expression `fun f -> const f $ arg1 $ arg2 $ arg3` **)
let make_term_expression ~loc fields =
  let field_names =
    fields
    |> List.map (fun { pld_name; _ } -> pld_name.txt)
    |> List.map (Ast_builder.Default.evar ~loc) in
  let rec aux field_names expr =
    match field_names with
    | [] -> expr
    | name :: tl ->
      let expr = [%expr Cmdliner.Term.app [%e expr] [%e name]] in
      aux tl expr in
  match field_names with
  | [] -> [%expr Cmdliner.Term.app (const f) ()]
  | _ -> aux field_names [%expr Term.const f]

type arg_type =
  | Argument      of argument
  | PositionalArg of positional_argument

and argument = { position : expression }

and positional_argument = {
  is_required : bool;
  default : expression option;
  env : expression option;
}

let make_conv_expr ~loc name position default env assertion doc is_required
    to_string of_string =
  let name_expr = Ast_builder.Default.estring ~loc name in
  let arg_type =
    match position with
    (* if the argument hasn't any position it's a positional argument *)
    | Some position ->
      Argument { position = Ast_builder.Default.eint ~loc position }
    | None ->
      PositionalArg
        {
          is_required;
          default;
          env =
            (if env then
               Some name
            else
              None)
            |> Option.map StringLabels.uppercase_ascii
            |> Option.map (Ast_builder.Default.estring ~loc);
        } in
  let to_string =
    match arg_type with
    | PositionalArg { is_required = true; _ }
    | Argument _ ->
      to_string
    | PositionalArg { is_required = false; _ } ->
      [%expr Option.fold ~none:"None" ~some:[%e to_string]] in
  let of_string =
    match arg_type with
    | Argument _
    | PositionalArg { is_required = true; _ } ->
      [%expr
        fun string ->
          [%e of_string] string
          |> Option.to_result
               ~none:
                 (`Msg
                   (Format.sprintf
                      "Cannot parse parameter \"%s\" from string: \"%s\""
                      [%e name_expr] string))]
    | PositionalArg { is_required = false; _ } ->
      [%expr fun string -> [%e of_string] string |> Result.ok] in
  let info =
    match arg_type with
    | PositionalArg { env = Some env; _ } ->
      [%expr info [[%e name_expr]] ~doc ~docv ~env:(Cmd.Env.info [%e env] ~doc)]
    | PositionalArg { env = None; _ } ->
      [%expr info [[%e name_expr]] ~doc ~docv]
    | Argument _ -> [%expr info [] ~doc ~docv] in
  let return =
    match arg_type with
    | PositionalArg { is_required = true; default = Some default; _ } ->
      [%expr required & opt (some arg_parser) (Some [%e default]) & [%e info]]
    | PositionalArg { is_required = false; default = Some default; _ } ->
      [%expr value & opt arg_parser [%e default] & [%e info]]
    | PositionalArg { is_required = true; default = None; _ } ->
      [%expr required & opt (some arg_parser) None & [%e info]]
    | PositionalArg { is_required = false; default = None; _ } ->
      [%expr value & opt arg_parser None & [%e info]]
    | Argument { position } ->
      [%expr required & pos [%e position] (some arg_parser) None & [%e info]]
  in
  [%expr
    let printer fmt arg = Format.fprintf fmt "%s" ([%e to_string] arg) in
    let parser str =
      str |> [%e of_string] |> fun res ->
      Result.bind res (fun result ->
          if [%e assertion] result then
            Ok result
          else
            Error (`Msg "Assertion failed")) in
    let open Arg in
    let docv = [%e name |> Ast_builder.Default.estring ~loc] in
    let doc = [%e doc] in
    let arg_parser = conv ~docv (parser, printer) in
    [%e return]]

let make_converter_expr ~loc label_declaration position =
  let { pld_name; pld_type; _ } = label_declaration in
  let name = pld_name.txt in
  let default = Attribute.get default_attribute label_declaration in
  let env = Attribute.get env_attribute label_declaration |> Option.is_some in
  let assertion =
    match Attribute.get assert_attribute label_declaration with
    | Some assertion -> assertion
    | None -> [%expr fun _ -> true] in
  let doc =
    Attribute.get doc_attribute label_declaration
    |> Option.value ~default:(Ast_builder.Default.estring ~loc "") in

  let rec expression_of_pld_type ~loc ?(is_option = false) pld_type =
    match pld_type with
    | [%type: int] ->
      make_conv_expr ~loc name position default env assertion doc
        (not is_option) [%expr string_of_int]
        [%expr fun str -> int_of_string_opt str]
    | [%type: string] ->
      make_conv_expr ~loc name position default env assertion doc
        (not is_option)
        [%expr fun x -> x]
        [%expr Option.some]
    | [%type: bool] ->
      make_conv_expr ~loc name position default env assertion doc
        (not is_option) [%expr string_of_bool]
        [%expr fun x -> bool_of_string_opt x]
    | [%type: [%t? pld_type] option] ->
      expression_of_pld_type ~loc ~is_option:true pld_type
    | { ptyp_desc = Ptyp_constr ({ txt = longident; _ }, _args); _ } ->
      let of_string =
        longident
        |> Astlib.Longident.flatten
        |> List.map (fun exp -> if exp = "t" then "of_string" else exp)
        (* TODO: find a better way for replacing "t" to "of_string" *)
        |> String.concat "."
        |> Ast_builder.Default.evar ~loc in
      let to_string =
        longident
        |> Astlib.Longident.flatten
        |> List.map (fun exp -> if exp = "t" then "to_string" else exp)
        (* TODO: same *)
        |> String.concat "."
        |> Ast_builder.Default.evar ~loc in
      make_conv_expr ~loc name position default env assertion doc
        (not is_option) to_string of_string
    | _ ->
      Location.raise_errorf ~loc "No support for this type: %s"
        (string_of_core_type pld_type) in

  expression_of_pld_type ~loc pld_type

(** the position of the argument is not the label position in the record, we have to compute it. **)
let get_positions label_declarations =
  let is_positional label_declaration =
    let default = Attribute.get default_attribute label_declaration in
    let env = Attribute.get env_attribute label_declaration in
    let is_optional_type =
      match label_declaration.pld_type with
      | [%type: [%t? _] option] -> true
      | _ -> false in
    is_optional_type || Option.is_some default || Option.is_some env in

  label_declarations
  |> List.mapi (fun index label_decl -> (index, label_decl))
  |> List.fold_left
       (fun (counter, positions) (index, element) ->
         if is_positional element then
           (counter + 1, None :: positions)
         else
           (counter, Some (index - counter) :: positions))
       (0, [])
  |> snd
  |> List.rev
  |> List.combine label_declarations

(* We want to generate a function that can be evaluate by cmdliner *)
let make_term_stri ~loc fields =
  fields
  |> get_positions
  |> List.map (fun (label_declaration, position) ->
         let conv_name_pat =
           Pat.var { loc; txt = label_declaration.pld_name.txt } in
         let conv_expr = make_converter_expr ~loc label_declaration position in
         (conv_name_pat, conv_expr))
  |> List.fold_left
       (fun converters_expr (converter_pat, converter_expr) ->
         [%expr
           let [%p converter_pat] = [%e converter_expr] in
           [%e converters_expr]])
       (make_term_expression ~loc fields)
  |> fun expression -> [%stri let term f = [%e expression]]

(* We want to generate the info function *)
let make_info_stri ~loc attributes =
  let values =
    attributes
    |> List.filter_map (fun attr ->
           match attr.attr_payload with
           | PStr structure ->
             if List.length structure = 1 then
               Some (attr.attr_name.txt, List.hd structure)
             else
               None
           | _ -> None)
    |> List.filter_map (fun (name, structure_item) ->
           match structure_item.pstr_desc with
           | Pstr_eval
               ({ pexp_desc = Pexp_constant (Pconst_string (str, _, _)); _ }, _)
             ->
             Some (name, str)
           | _ -> None) in

  let name =
    List.find_opt (fun (name, _) -> name = "cmdliner.name") values
    |> Option.map snd
    |> Option.value ~default:"command"
    |> Ast_builder.Default.estring ~loc in

  let doc =
    List.find_opt (fun (name, _) -> name = "cmdliner.doc") values
    |> Option.map snd in

  let desc =
    List.find_opt (fun (name, _) -> name = "cmdliner.desc") values
    |> Option.map snd in

  let expr =
    match (doc, desc) with
    | Some doc, Some desc ->
      [%expr
        let doc = [%e Ast_builder.Default.estring ~loc doc] in
        let man =
          [
            `S Manpage.s_description;
            `P [%e Ast_builder.Default.estring ~loc desc];
            `S Manpage.s_bugs;
            `P "Email bug reports to <contact@marigold.dev>.";
          ] in
        Cmd.info [%e name] ~doc ~man]
    | Some doc, None ->
      [%expr
        let doc = [%e Ast_builder.Default.estring ~loc doc] in
        let man =
          [`S Manpage.s_bugs; `P "Email bug reports to <contact@marigold.dev>."]
        in
        Cmd.info [%e name] ~doc ~man]
    | None, Some desc ->
      [%expr
        let man =
          [
            `S Manpage.s_description;
            `P [%e Ast_builder.Default.estring ~loc desc];
            `S Manpage.s_bugs;
            `P "Email bug reports to <contact@marigold.dev>.";
          ] in
        Cmd.info [%e name] ~man]
    | None, None -> [%expr Cmd.info [%e name]] in

  [%stri let info = [%e expr]]

let generate_impl ~ctxt (_rec_flag, type_declarations) =
  let loc = Expansion_context.Deriver.derived_item_loc ctxt in
  type_declarations
  |> List.map (fun type_decl ->
         match type_decl with
         | { ptype_kind = Ptype_record fields; ptype_attributes; _ } ->
           let info = make_info_stri ~loc ptype_attributes in
           let term = make_term_stri ~loc fields in
           [info; term]
         | _ ->
           Location.raise_errorf ~loc "Cannot derive anything for this type")
  |> List.flatten

let str_type_decl_generator = Deriving.Generator.V2.make_noarg generate_impl

let cmdliner_deriver =
  Deriving.add "cmdliner" ~str_type_decl:str_type_decl_generator
