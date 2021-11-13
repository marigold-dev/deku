open Region

let byte =
  Lexing.
    {pos_fname= "a dummy file name"; pos_lnum= 20; pos_bol= 25; pos_cnum= 30}

let default_location =
  Region.make
    ~start:(Pos.make ~byte ~point_num:10 ~point_bol:10)
    ~stop:(Pos.make ~byte ~point_num:20 ~point_bol:10)

let default_region1 =
  Region.make
    ~start:(Pos.make ~byte ~point_num:10 ~point_bol:10)
    ~stop:(Pos.make ~byte ~point_num:20 ~point_bol:10)

let error display_format error =
  let buffer = Buffer.create 100 in
  let formatter = Format.formatter_of_buffer buffer in
  Formatter.error_ppformat ~display_format formatter error ;
  Format.pp_print_flush formatter () ;
  print_string (Buffer.contents buffer)

let human_readable_error e =
  let display_format = Display.human_readable in
  let display_format =
    match display_format with
    | Ex_display_format (Human_readable as a) ->
        a
    | _ ->
        assert false
  in
  error display_format e

let%expect_test "main" =
  human_readable_error (`Main_invalid_syntax_name "foo") ;
  [%expect
    {|
      Invalid syntax option: 'foo'.
      Use 'pascaligo', 'cameligo', or 'reasonligo'.|}] ;
    human_readable_error (`Main_invalid_dialect_name "foo") ;
  [%expect
    {|
      Invalid dialect option: 'foo'.
      Use 'verbose' or 'terse'.|}] ;
  human_readable_error (`Main_invalid_extension "foo") ;
  [%expect
    {|
  Invalid file extension 'foo'.
  Use '.ligo' for PascaLIGO, '.mligo' for CameLIGO, '.religo' for ReasonLIGO, or the --syntax option.|}] ;
  human_readable_error
    (`Main_unparse_tracer
      [`Tezos_alpha_error Tezos_error_monad.Error_monad.Timeout]) ;
  [%expect {|
    Error(s) occurred while translating to Michelson:
    The request has timed out|}] ;
  human_readable_error
    (`Main_typecheck_contract_tracer
      ( let open Tezos_micheline.Micheline in
        root (strip_locations Michelson.t_unit),
        [`Tezos_alpha_error Tezos_error_monad.Error_monad.Canceled] )) ;
  [%expect {|
  Error(s) occurred while type checking the contract:
  The promise was unexpectedly canceled
  |}] ;
  human_readable_error
    (`Main_could_not_serialize [`Tezos_alpha_error Tezos_error_monad.Error_monad.Canceled]) ;
  [%expect {|
    Error(s) occurred while serializing Michelson code:
    The promise was unexpectedly canceled|}] ;
  human_readable_error `Main_unknown_failwith_type ;
  [%expect {|The contract failed to dry run, and returned an unsupported failwith type. Only int, string, and bytes are supported as failwith types for dry-run.|}] ;
  human_readable_error `Main_unknown ;
  [%expect {|An unknown error occurred.|}];
  human_readable_error `Main_entrypoint_not_a_function ;
  [%expect {|
    Invalid command line argument.
    The provided entrypoint is not a function.|}] ;
  human_readable_error `Main_entrypoint_not_found ;
  [%expect {|
    Invalid command line argument.
    The provided entrypoint is not found in the contract.|}] ;
  human_readable_error (`Main_invalid_amount "foo") ;
  [%expect {|
    Invalid command line option "--amount".
    The provided amount "foo" is invalid. Use an integer instead.|}] ;
  human_readable_error (`Main_invalid_balance "foo") ;
  [%expect {|
    Invalid command line option "--balance".
    The provided balance "foo" is invalid. Use an integer instead.|}] ;
  human_readable_error (`Main_invalid_source "foo") ;
  [%expect {|
    Invalid command line option "--source".
    The provided source address "foo" is invalid. A valid Tezos address is a string prefixed by either tz1, tz2, tz3 or KT1 and followed by a Base58 encoded hash and terminated by a 4-byte checksum.|}];
  human_readable_error (`Main_invalid_sender "foo") ;
  [%expect {|
    Invalid command line option "--sender".
    The provided sender address "foo" is invalid. A valid Tezos address is a string prefixed by either tz1, tz2, tz3 or KT1 and followed by a Base58 encoded hash and terminated by a 4-byte checksum.|}]

let%expect_test "main_execution_failed" =
  let error e = human_readable_error (`Main_execution_failed e) in
  error (Simple_utils.Runned_result.Failwith_int 743) ;
  [%expect {|An error occurred while evaluating an expression: 743|}] ;
  error (Simple_utils.Runned_result.Failwith_string "bar") ;
  [%expect {|An error occurred while evaluating an expression: bar|}] ;
  error (Simple_utils.Runned_result.Failwith_bytes (Bytes.of_string "bar")) ;
  [%expect {|An error occurred while evaluating an expression: bar|}]

let%expect_test _ =
  human_readable_error (`Main_unparse_michelson_result [`Tezos_alpha_error Tezos_error_monad.Error_monad.Timeout; `Tezos_alpha_error Tezos_error_monad.Error_monad.Canceled]) ;
  [%expect {|
  Error(s) occurred while unparsing the Michelson result:
  The request has timed out
  The promise was unexpectedly canceled
  |}] ;
  human_readable_error (`Main_parse_michelson_input [`Tezos_alpha_error Tezos_error_monad.Error_monad.Timeout; `Tezos_alpha_error Tezos_error_monad.Error_monad.Canceled]) ;
  [%expect {|
  Error(s) occurred while parsing the Michelson input:
  The request has timed out
  The promise was unexpectedly canceled
  |}] ;
  human_readable_error (`Main_parse_michelson_code [`Tezos_alpha_error Tezos_error_monad.Error_monad.Timeout; `Tezos_alpha_error Tezos_error_monad.Error_monad.Canceled]) ;
  [%expect {|
  Error(s) occurred while checking the contract:
  The request has timed out
  The promise was unexpectedly canceled
  |}] ;
  human_readable_error (`Main_michelson_execution_error [`Tezos_alpha_error Tezos_error_monad.Error_monad.Timeout; `Tezos_alpha_error Tezos_error_monad.Error_monad.Canceled]) ;
  [%expect {|
  Error(s) occurred while executing the contract:
  The request has timed out
  The promise was unexpectedly canceled
  |}]

let%expect_test "pretty" = () (* not used *)

let%expect_test "self_ast_imperative" =
  let open Ast_imperative in
  let open Location in
  let error e = human_readable_error (`Main_self_ast_imperative e) in
  let location_t = File default_location in
  let type_content = T_variable (Var.of_name "foo") in
  let type_expression = {type_content; location= location_t} in
  let expression_content = E_skip in
  let expression = {expression_content; location= location_t} in
  error (`Self_ast_imperative_long_constructor ("foo", type_expression)) ;
  [%expect
    {|
  File "a dummy file name", line 20, character 5:

  Ill-formed data constructor "foo".
  Data constructors have a maximum length of 32 characters, which is a limitation imposed by annotations in Tezos.
  |}] ;
  error (`Self_ast_imperative_bad_timestamp ("bar", expression)) ;
  [%expect
    {|
  File "a dummy file name", line 20, character 5:
   Ill-formed timestamp "bar".
  At this point, a string with a RFC3339 notation or the number of seconds since Epoch is expected.
  |}] ;
  error (`Self_ast_imperative_bad_format_literal expression) ;
  [%expect
    {|
  File "a dummy file name", line 20, character 5:
   Ill-formed literal "skip".
  In the case of an address, a string is expected prefixed by either tz1, tz2, tz3 or KT1 and followed by a Base58 encoded hash and terminated by a 4-byte checksum.
  In the case of a key_hash, signature, or key a Base58 encoded hash is expected.
  |}] ;
  error (`Self_ast_imperative_bad_empty_arity (C_INT, expression)) ;
  [%expect
    {|
  File "a dummy file name", line 20, character 5:
   Ill-formed "INT" expression.
  No functions arguments are expected.
  |}] ;
  error (`Self_ast_imperative_bad_single_arity (C_ASSERTION, expression)) ;
  [%expect
    {|
  File "a dummy file name", line 20, character 5:

  Ill-formed "ASSERTION" expression
  One function argument is expected.
  |}] ;
  error (`Self_ast_imperative_bad_map_param_type (C_EDIV, expression)) ;
  [%expect
    {|
  File "a dummy file name", line 20, character 5:

  Ill-formed "EDIV" expression.
  A list of pair parameters is expected.
  |}] ;
  error (`Self_ast_imperative_bad_set_param_type (C_ITER, expression)) ;
  [%expect
    {|
  File "a dummy file name", line 20, character 5:

  Ill-formed "ITER" expression.
  A list of pair parameters is expected.
  |}] ;
  error (`Self_ast_imperative_bad_convertion_bytes expression) ;
  [%expect
    {|
  File "a dummy file name", line 20, character 5:
   Ill-formed bytes literal.
  Example of a valid bytes literal: "ff7a7aff".
  |}]

let%expect_test _ =
  human_readable_error (`Main_purification (`purification_corner_case "foo")) ;
  [%expect {|Corner case: foo|}] ;
  human_readable_error (`Main_depurification (`purification_corner_case "foo")) ;
  [%expect {| |}]

let%expect_test "desugaring" = ()

let%expect_test "sugaring" = ()

let%expect_test "main_cit_pascaligo" =
  let open Cst.Pascaligo in
  let open Location in
  let ghost = 
    object 
      method region = Region.ghost 
      method attributes = []
      method payload = ""
    end 
  in
  let error e = human_readable_error (`Main_cit_pascaligo e) in
  let lexeme_reg : lexeme reg = {value= "foo"; region= default_region1} in
  let pvar = PVar {value = {variable = lexeme_reg ; attributes = []} ; region = default_region1} in
  let type_expr = TString {value= "yolo"; region= default_region1} in
  let location_t = File default_location in
  error (`Concrete_pascaligo_unsupported_pattern_type pvar) ;
  [%expect
    {|
      File "a dummy file name", line 20, character 5:

      Invalid case pattern.
      Can't match on values.|}] ;
  error (`Concrete_pascaligo_unsupported_string_singleton type_expr) ;
  [%expect
    {|
      File "a dummy file name", line 20, character 5:

      Invalid type.
      It's not possible to assign a string to a type.|}] ;
  error (`Concrete_pascaligo_michelson_type_wrong (type_expr, "foo")) ;
  [%expect
    {|
  File "a dummy file name", line 20, character 5:

  Invalid "foo" type.
  At this point, an annotation, in the form of a string, is expected for the preceding type.
  |}] ;
  error (`Concrete_pascaligo_michelson_type_wrong_arity (location_t, "zzz")) ;
  [%expect
    {|
      File "a dummy file name", line 20, character 5:

      Invalid "zzz" type.
      An even number of 2 or more arguments is expected, where each odd item is a type annotated by the following string.|}] ;
  error (`Concrete_pascaligo_recursive_fun location_t) ;
  [%expect
    {|
  File "a dummy file name", line 20, character 5:

  Invalid function declaration.
  Recursive functions are required to have a type annotation (for now).
  |}] ;
  let pvar : var_pattern = {variable = {value = "foo"; region = Region.ghost} ; attributes = []} in
  error
    (`Concrete_pascaligo_block_attribute
      { value=
          { enclosing= BeginEnd (ghost, ghost);
            statements=
              ( Data
                  (LocalVar
                     { value=
                         { kwd_var= ghost;
                           pattern= PVar {value= pvar; region= Region.ghost};
                           var_type= None;
                           assign= ghost;
                           init= EVar {value= "xxx"; region= Region.ghost};
                           terminator= None;
                           attributes = []};
                       region= Region.ghost }),
                [] );
            terminator= None };
        region= Region.ghost }) ;
  [%expect
    {|
  Invalid attribute declaration.
  Attributes have to follow the declaration it is attached to.
  |}]

let%expect_test "main_cit_cameligo" =
  let open Cst.Cameligo in
  let open Location in
  let error e = human_readable_error (`Main_cit_cameligo e) in
  let variable = {value= "dog"; region= default_region1} in
  let pvar = PVar {value = { variable ; attributes = []} ; region = default_region1} in
  let type_expr = TVar {value= "dog"; region= default_region1} in
  let location_t = File default_location in
  error (`Concrete_cameligo_recursive_fun default_region1) ;
  [%expect
    {|
      File "a dummy file name", line 20, character 5:

      Invalid function declaration.
      Recursive functions are required to have a type annotation (for now).|}] ;
  error (`Concrete_cameligo_unsupported_pattern_type [pvar]) ;
  [%expect
    {|
      File "a dummy file name", line 20, character 5:

      Invalid pattern.
      Can't match on values.|}] ;
  error (`Concrete_cameligo_unsupported_string_singleton type_expr) ;
  [%expect
    {|
      File "a dummy file name", line 20, character 5:

      Invalid type.
      It's not possible to assign a string to a type.|}] ;
  error (`Concrete_cameligo_recursion_on_non_function location_t) ;
  [%expect {|
    File "a dummy file name", line 20, character 5:

    Invalid let declaration.
    Only functions can be recursive.|}] ;
  error (`Concrete_cameligo_michelson_type_wrong (type_expr, "foo")) ;
  [%expect
    {|
File "a dummy file name", line 20, character 5:

Invalid "foo" type.
At this point, an annotation, in the form of a string, is expected for the preceding type.
|}] ;
  error (`Concrete_cameligo_michelson_type_wrong_arity (location_t, "zzz")) ;
  [%expect
    {|
      File "a dummy file name", line 20, character 5:

      Invalid "zzz" type.
      An even number of 2 or more arguments is expected, where each odd item is a type annotated by the following string.|}];
  error(`Concrete_cameligo_missing_funarg_annotation variable);
  [%expect
    {|
      File "a dummy file name", line 20, character 5:

      Missing a type annotation for argument "dog".|}]

let%expect_test "main_cit_reasonligo" =
  let open Cst.Reasonligo in
  let open Location in
  let error e = human_readable_error (`Main_cit_reasonligo e) in
  let variable = {value= "dog"; region= default_region1} in
  let pvar = PVar  {value= {variable; attributes = []}; region= default_region1} in
  let type_expr = TVar {value= "dog"; region= default_region1} in
  let location_t = File default_location in
  error (`Concrete_reasonligo_recursive_fun default_region1) ;
  [%expect
    {|
      File "a dummy file name", line 20, character 5:

      Invalid function declaration.
      Recursive functions are required to have a type annotation (for now).|}] ;
  error (`Concrete_reasonligo_unsupported_pattern_type pvar) ;
  [%expect
    {|
      File "a dummy file name", line 20, character 5:

      Invalid pattern matching.
      Can't match on values.|}] ;
  error (`Concrete_reasonligo_unsupported_string_singleton type_expr) ;
  [%expect
    {|
      File "a dummy file name", line 20, character 5:

      Invalid type.
      It's not possible to assign a string to a type.|}] ;
  error (`Concrete_reasonligo_recursion_on_non_function location_t) ;
  [%expect {|
    File "a dummy file name", line 20, character 5:

    Invalid let declaration.
    Only functions can be recursive.|}] ;
  error (`Concrete_reasonligo_michelson_type_wrong (type_expr, "foo")) ;
  [%expect
    {|
        File "a dummy file name", line 20, character 5:

        Invalid "foo" type.
        At this point, an annotation, in the form of a string, is expected for the preceding type.|}] ;
  error (`Concrete_reasonligo_michelson_type_wrong_arity (location_t, "bar")) ;
  [%expect
    {|
      File "a dummy file name", line 20, character 5:

      Invalid "bar" type.
      An even number of 2 or more arguments is expected, where each odd item is a type annotated by the following string.|}];
  error(`Concrete_reasonligo_missing_funarg_annotation variable);
  [%expect
    {|
      File "a dummy file name", line 20, character 5:

      Missing a type annotation for argument "dog".|}]


let%expect_test "typer" =
  let open Ast_typed in
  let open Location in
  let error e = human_readable_error (`Main_checking e) in
  let location_t = File default_location in
  let environment = Environment.empty in
  let type_variable = Var.of_name "foo" in
  let expression_variable = Location.wrap (Var.of_name "bar") in
  let ast_core_expression_variable : Ast_core.expression_variable =
    Location.wrap (Var.of_name "bar")
  in
  let ast_core_expression_content : Ast_core.expression_content =
    E_variable ast_core_expression_variable
  in
  let ast_core_expression : Ast_core.expression =
    {expression_content= ast_core_expression_content; sugar= None; location= location_t}
  in
  let type_expression : Ast_typed.type_expression =
    { type_content= T_variable (Var.of_name "foo");
      type_meta= None;
      orig_var = None ;
      location= File default_location }
  in
  let type_expression2 : Ast_typed.type_expression =
    { type_content= T_variable (Var.of_name "bar");
      type_meta= None;
      orig_var = None ;
      location= File default_location }
  in
  let ast_core_constant = C_INT in
  let expression_content = E_literal Literal_unit in
  let expression =
    {expression_content; location= location_t; type_expression}
  in
  error (`Typer_michelson_comb_no_record location_t) ;
  [%expect
    {|
      File "a dummy file name", line 20, character 5:

      Invalid usage of type "michelson_pair".
      The "michelson_pair" type expects a record type as argument.|}] ;
  error (`Typer_michelson_comb_no_variant location_t) ;
  [%expect
    {|
      File "a dummy file name", line 20, character 5:

      Invalid usage of type "michelson_or".
      The "michelson_or" type expects a variant type as argument.|}] ;
  error (`Typer_unbound_type_variable (environment, type_variable, location_t)) ;
  [%expect
    {|
    File "a dummy file name", line 20, character 5:

    Type "foo" not found.|}] ;
  error
    (`Typer_unbound_variable (environment, expression_variable, location_t)) ;
  [%expect
    {|
      File "a dummy file name", line 20, character 5:

      Variable "bar" not found.|}] ;
  error (`Typer_match_missing_case ([Label "Foo"; Label "Bar"; Label "Cat"], [Label "Bar"], location_t)) ;
  [%expect {|
  File "a dummy file name", line 20, character 5:

  Pattern matching is not exhaustive.
  Cases that are missing: Cat, Foo.|}] ;
  error (`Typer_match_extra_case ([Label "Foo"], [Label "Foo"; Label "Foo"; Label "Bar"], location_t)) ;
  [%expect {|
    File "a dummy file name", line 20, character 5:

    Pattern matching over too many cases.
    These case(s) are duplicate:
    Foo
    These case(s) don't belong to the variant:
    Bar
    Please remove the extra cases.|}] ;
  error
    (`Typer_unbound_constructor
      (environment, Label "Some-Constructor", location_t)) ;
  [%expect
    {|
    File "a dummy file name", line 20, character 5:

    Constructor "Some-Constructor" not found.|}] ;
  error
    (`Typer_michelson_or_no_annotation (Label "Some-Constructor", location_t)) ;
  [%expect
    {|
    File "a dummy file name", line 20, character 5:

    Incorrect usage of type "michelson_or".
    The contructor "Some-Constructor" must be annotated with a variant type.|}] ;
  error
    (`Typer_module_tracer
      ([], `Typer_michelson_comb_no_variant (File default_location))) ;
  [%expect
    {|
    File "a dummy file name", line 20, character 5:

    Invalid usage of type "michelson_or".
    The "michelson_or" type expects a variant type as argument.|}] ;
  error
    (`Typer_module_tracer ([], `Typer_michelson_comb_no_variant location_t)) ;
  [%expect
    {|
    File "a dummy file name", line 20, character 5:

    Invalid usage of type "michelson_or".
    The "michelson_or" type expects a variant type as argument.|}] ;
  error
    (`Typer_constant_declaration_tracer
      ( ast_core_expression_variable,
        ast_core_expression,
        Some type_expression,
        `Typer_michelson_comb_no_record location_t )) ;
  [%expect
    {|
    File "a dummy file name", line 20, character 5:

    Invalid usage of type "michelson_pair".
    The "michelson_pair" type expects a record type as argument.|}] ;
  error (`Typer_needs_annotation (ast_core_expression, "foo")) ;
  [%expect
    {|
  File "a dummy file name", line 20, character 5:

  Missing type annotation.
  'foo' needs to be annotated with a type.|}] ;
  error
    (`Typer_fvs_in_create_contract_lambda
      (ast_core_expression, expression_variable)) ;
  [%expect
    {|
  File "a dummy file name", line 20, character 5:

  Free variable 'bar' is not allowed in CREATE_CONTRACT lambda|}] ;
  error
    (`Typer_create_contract_lambda (ast_core_constant, ast_core_expression)) ;
  [%expect
    {|
  File "a dummy file name", line 20, character 5:

  Invalid usage of Tezos.create_contract.
  The first argument must be an inline function.|}] ;
  error
    (`Typer_should_be_a_function_type (type_expression, ast_core_expression)) ;
  [%expect
    {|
  File "a dummy file name", line 20, character 5:

  Invalid type.
  Expected a function type, but got "foo".|}] ;
  error
    (`Typer_bad_record_access
      (Label "bar", ast_core_expression, type_expression, location_t)) ;
  [%expect
    {|
  File "a dummy file name", line 20, character 5:

  Invalid record field "bar" in record "bar".|}] ;
  error
    (`Typer_expression_tracer
      ( ast_core_expression,
        `Typer_bad_record_access
          (Label "bar", ast_core_expression, type_expression, location_t) )) ;
  [%expect
    {|
  File "a dummy file name", line 20, character 5:

  Invalid record field "bar" in record "bar".|}] ;
  error
    (`Typer_record_access_tracer
      ( expression,
        `Typer_bad_record_access
          (Label "bar", ast_core_expression, type_expression, location_t) )) ;
  [%expect
    {|
  File "a dummy file name", line 20, character 5:

  Invalid record field "bar" in record "bar".|}] ;
  error (`Typer_assert_equal (location_t, type_expression, type_expression2)) ;
  [%expect {|
    File "a dummy file name", line 20, character 5:

    Invalid type(s).
    Expected: "foo", but got: "bar".|}] ;
  error (`Typer_corner_case "foo") ;
  [%expect {|
    A type system corner case occurred:
    foo|}] ;
  error (`Typer_bad_collect_loop (type_expression, location_t)) ;
  [%expect
    {|
      File "a dummy file name", line 20, character 5:

      Bounded loop over a value with an incorrect type.
      Expected a value with type: "list", "set" or "map", but got a value of type "foo".|}] ;
  error (`Typer_declaration_order_record location_t) ;
  [%expect
    {|
      File "a dummy file name", line 20, character 5:

      Incorrect argument provided to Layout.convert_to_(left|right)_comb.
      The given argument must be annotated with the type of the value.|}] ;
  error (`Typer_too_small_record location_t) ;
  [%expect
    {|
      File "a dummy file name", line 20, character 5:

      Incorrect argument provided to Layout.convert_to_(left|right)_comb.
      The record must have at least two elements.|}] ;
  error (`Typer_expected_record (location_t, type_expression)) ;
  [%expect
    {|
      File "a dummy file name", line 20, character 5:

      Invalid argument.
      Expected a record, but got an argument of type "foo".|}] ;
  error (`Typer_expected_variant (location_t, type_expression)) ;
  [%expect
    {|
      File "a dummy file name", line 20, character 5:

      Invalid argument.
      Expected a variant, but got an argument of type "foo".|}] ;
  error
    (`Typer_wrong_param_number
      (location_t, "foo", 10, [type_expression; type_expression2])) ;
  [%expect
    {|
      File "a dummy file name", line 20, character 5:

      Function "foo" called with wrong number of arguments.
      Expected 10 arguments, got 2 arguments.|}] ;
  error (`Typer_expected_function (location_t, type_expression)) ;
  [%expect
    {|
    File "a dummy file name", line 20, character 5:

    Invalid argument.
    Expected a function, but got an argument of type "foo".|}] ;
  error (`Typer_expected_pair (location_t, type_expression)) ;
  [%expect
    {|
    File "a dummy file name", line 20, character 5:

    Incorrect argument.
    Expected a pair, but got an argument of type "foo".|}] ;
  error (`Typer_expected_list (location_t, type_expression)) ;
  [%expect
    {|
    File "a dummy file name", line 20, character 5:

    Incorrect argument.
    Expected a list, but got an argument of type "foo".|}] ;
  error (`Typer_expected_set (location_t, type_expression)) ;
  [%expect
    {|
    File "a dummy file name", line 20, character 5:

    Incorrect argument.
    Expected a set, but got an argument of type "foo".|}] ;
  error (`Typer_expected_map (location_t, type_expression)) ;
  [%expect
    {|
    File "a dummy file name", line 20, character 5:

    Incorrect argument.
    Expected a map, but got an argument of type "foo".|}] ;
  error (`Typer_expected_big_map (location_t, type_expression)) ;
  [%expect
    {|
    File "a dummy file name", line 20, character 5:

    Incorrect argument.
    Expected a big_map, but got an argument of type "foo".|}] ;
  error (`Typer_expected_option (location_t, type_expression)) ;
  [%expect
    {|
    File "a dummy file name", line 20, character 5:

    Incorrect argument.
    Expected an option, but got an argument of type "foo".|}] ;
  error (`Typer_expected_nat (location_t, type_expression)) ;
  [%expect
    {|
    File "a dummy file name", line 20, character 5:

    Incorrect argument.
    Expected a nat, but got an argument of type "foo".|}] ;
  error (`Typer_expected_bytes (location_t, type_expression)) ;
  [%expect
    {|
    File "a dummy file name", line 20, character 5:

    Incorrect argument.
    Expected bytes, but got an argument of type "foo".|}] ;
  error (`Typer_expected_key (location_t, type_expression)) ;
  [%expect
    {|
    File "a dummy file name", line 20, character 5:

    Incorrect argument.
    Expected a key, but got an argument of type "foo".|}] ;
  error (`Typer_expected_signature (location_t, type_expression)) ;
  [%expect
    {|
    File "a dummy file name", line 20, character 5:

    Incorrect argument.
    Expected a signature, but got an argument of type "foo".|}] ;
  error (`Typer_expected_contract (location_t, type_expression)) ;
  [%expect
    {|
    File "a dummy file name", line 20, character 5:

    Incorrect argument.
    Expected a contract, but got an argument of type "foo".|}] ;
  error (`Typer_expected_string (location_t, type_expression)) ;
  [%expect
    {|
    File "a dummy file name", line 20, character 5:

    Incorrect argument.
    Expected a string, but got an argument of type "foo".|}] ;
  error (`Typer_expected_key_hash (location_t, type_expression)) ;
  [%expect
    {|
    File "a dummy file name", line 20, character 5:

    Incorrect argument.
    Expected a key hash, but got an argument of type "foo".|}] ;
  error (`Typer_expected_mutez (location_t, type_expression)) ;
  [%expect
    {|
    File "a dummy file name", line 20, character 5:

    Incorrect argument.
    Expected a mutez, but got an argument of type "foo".|}] ;
  error (`Typer_expected_op_list (location_t, type_expression)) ;
  [%expect
    {|
    File "a dummy file name", line 20, character 5:

    Incorrect argument.
    Expected a list of operations, but got an argument of type "foo".|}] ;
  error (`Typer_expected_int (location_t, type_expression)) ;
  [%expect
    {|
    File "a dummy file name", line 20, character 5:

    Incorrect argument.
    Expected an int, but got an argument of type "foo".|}] ;
  error (`Typer_expected_bool (location_t, type_expression)) ;
  [%expect
    {|
    File "a dummy file name", line 20, character 5:

    Incorrect argument.
    Expected a boolean, but got an argument of type "foo".|}] ;
  error (`Typer_not_matching (location_t, type_expression, type_expression2)) ;
  [%expect
    {|
    File "a dummy file name", line 20, character 5:

    These types are not matching:
     - foo
     - bar|}] ;
  error (`Typer_not_annotated location_t);
  [%expect {|
    File "a dummy file name", line 20, character 5:

    Can't infer the type of this value, please add a type annotation.|}] ;
  error (`Typer_bad_substraction location_t);
  [%expect {|
    File "a dummy file name", line 20, character 5:

    Invalid subtraction.
    The following forms of subtractions are possible:
      * timestamp - int = timestamp
      * timestamp - timestamp = int
      * int/nat - int/nat = int
      * mutez/tez - mutez/tez = mutez.|}] ;
  error (`Typer_wrong_size (location_t, type_expression)) ;
  [%expect
    {|
    File "a dummy file name", line 20, character 5:

    Incorrect value applied.
    A value with one of the following types is expected: map, list, string, byte or set.|}] ;
  error (`Typer_wrong_neg (location_t, type_expression)) ;
  [%expect
    {|
    File "a dummy file name", line 20, character 5:

    Invalid value used for negation.
    Expected a value of type nat or int, but got foo.|}] ;
  error (`Typer_wrong_not (location_t, type_expression)) ;
  [%expect
    {|
    File "a dummy file name", line 20, character 5:

    Invalid value used for not operation.
    Expected a value of type Boolean, nat or int, but got foo.|}] ;
  error (`Typer_typeclass_error (location_t, [[type_expression; type_expression2 ]], [type_expression2])) ;
  [%expect
    {|
    File "a dummy file name", line 20, character 5:

    Invalid arguments.
    Expected an argument of type (foo, bar), but got an argument of type bar.|}] ;
  error (`Typer_converter type_expression) ;
  [%expect
    {|
    File "a dummy file name", line 20, character 5:

    Invalid usage of a Michelson converter.
    Converters can only be used on records or variants, but got foo.|}] ;
  error (`Typer_converter type_expression) ;
  [%expect
    {|
    File "a dummy file name", line 20, character 5:

    Invalid usage of a Michelson converter.
    Converters can only be used on records or variants, but got foo.|}] ;
  error (`Typer_uncomparable_types (location_t, type_expression, type_expression2)) ;
  [%expect {|
    File "a dummy file name", line 20, character 5:

    Invalid arguments.
    These types cannot be compared: "foo" and "bar".|}] ;
  error (`Typer_comparator_composed (location_t, type_expression)) ;
  [%expect
    {|
    File "a dummy file name", line 20, character 5:

    Invalid arguments.
    Only composed types of not more than two element are allowed to be compared.|}] ;
  error (`Typer_expected_ascription ast_core_expression) ;
  [%expect
    {|
    File "a dummy file name", line 20, character 5:

    Invalid argument.
    At this point a block of code is expected, but got "bar".|}] ;

  (* new typer errors *)
  error
  (`Typer_constant_decl_tracer
    ( ast_core_expression_variable,
      ast_core_expression,
      Some type_expression2,
      `Typer_comparator_composed (location_t, type_expression ))) ;
[%expect
  {|
  File "a dummy file name", line 20, character 5:

  Invalid arguments.
  Only composed types of not more than two element are allowed to be compared.|}] ;
  error
    (`Typer_different_types
      ( type_expression,
        type_expression2)) ;
  [%expect
    {|
    File "a dummy file name", line 20, character 5:

    This expression has type bar, but an expression was expected of type foo.|}]

let%expect_test "interpreter" = ()

let%expect_test "self_ast_typed" =
  let open Ast_typed in
  let open Location in
  let error e = human_readable_error (`Main_self_ast_typed e) in
  let expression_variable = Location.wrap (Var.of_name "bar") in
  let location_t = File default_location in
  let type_expression : Ast_typed.type_expression =
    { type_content= T_variable (Var.of_name "foo");
      type_meta= None ;
      orig_var = None ;
      location= File default_location }
  in
  let type_expression2 : Ast_typed.type_expression =
    { type_content= T_variable (Var.of_name "bar");
      type_meta= None ;
      orig_var = None ;
      location= File default_location }
  in
  let expression_content = E_literal Literal_unit in
  let expression =
    {expression_content; location= location_t; type_expression}
  in
  error (`Self_ast_typed_rec_call (expression_variable, location_t)) ;
  [%expect
    {|
    File "a dummy file name", line 20, character 5:

    Recursive call not in tail position.
    The value of a recursive call must be immediately returned by the defined function.|}] ;
  error
    (`Self_ast_typed_bad_self_type
      (type_expression, type_expression2, location_t)) ;
  [%expect
    {|
    File "a dummy file name", line 20, character 5:

    Invalid type annotation.
    "bar" was given, but "foo" was expected.
    Note that "Tezos.self" refers to this contract, so the parameters should be the same. |}] ;
  error (`Self_ast_typed_format_entrypoint_ann ("foo", location_t)) ;
  [%expect
    {|
    File "a dummy file name", line 20, character 5:

    Invalid entrypoint "foo". One of the following patterns is expected:
    * "%bar" is expected for entrypoint "Bar"
    * "%default" when no entrypoint is used. |}] ;
  error (`Self_ast_typed_entrypoint_ann_not_literal location_t) ;
  [%expect
    {|
    File "a dummy file name", line 20, character 5:

    Invalid entrypoint value.
    The entrypoint value must be a string literal. |}] ;
  error (`Self_ast_typed_unmatched_entrypoint location_t) ;
  [%expect
    {|
    File "a dummy file name", line 20, character 5:

    Invalid entrypoint value.
    The entrypoint value does not match a constructor of the contract parameter. |}] ;
  error (`Self_ast_typed_nested_big_map location_t) ;
  [%expect
    {|
    File "a dummy file name", line 20, character 5:

    Invalid big map nesting.
    A big map cannot be nested inside another big map. |}] ;
  error (`Self_ast_typed_corner_case "foo") ;
  [%expect {|
    Internal error: foo |}] ;
  error (`Self_ast_typed_contract_io ("foo", expression)) ;
  [%expect
    {|
    File "a dummy file name", line 20, character 5:

    Invalid type for entrypoint "foo".
    An entrypoint must of type "parameter * storage -> operations list * storage". |}] ;
  error
    (`Self_ast_typed_contract_list_ops ("foo", type_expression, expression)) ;
  [%expect
    {|
    File "a dummy file name", line 20, character 5:

    Invalid type for entrypoint "foo".
    An entrypoint must of type "parameter * storage -> operations list * storage".
    We expected a list of operations but we got foo |}] ;
  error
    (`Self_ast_typed_expected_same_entry
      ("foo", type_expression, type_expression2, expression)) ;
  [%expect
    {|
    File "a dummy file name", line 20, character 5:

    Invalid type for entrypoint "foo".
    The storage type "foo" of the function parameter must be the same as the storage type "bar" of the return value. |}] ;
  error (`Self_ast_typed_pair_in (location_t, `Contract)) ;
  [%expect
    {|
    File "a dummy file name", line 20, character 5:

    Invalid contract.
    Expected a tuple as argument. |}] ;
  error (`Self_ast_typed_pair_out location_t) ;
  [%expect
    {|
    File "a dummy file name", line 20, character 5:

    Invalid entrypoint.
    Expected a tuple of operations and storage as return value. |}]

let%expect_test "self_mini_c" =
  let error e = human_readable_error (`Main_self_mini_c e) in
  error (`Self_mini_c_bad_self_address C_SELF_ADDRESS) ;
  [%expect {|"Tezos.self" must be used directly and cannot be used via another function. |}] ;
  error `Self_mini_c_not_a_function ;
  [%expect {|
    Invalid type for entrypoint.
    An entrypoint must of type "parameter * storage -> operations list * storage". |}] ;
  error `Self_mini_c_aggregation ;
  [%expect {|
    Invalid type for entrypoint.
    An entrypoint must of type "parameter * storage -> operations list * storage". |}]

let%expect_test "spilling" =
  let error (e:Spilling.Errors.spilling_error) = human_readable_error (`Main_spilling e) in
  let open Ast_typed in
  let open Location in
  let type_variable : Ast_typed.type_variable = Var.of_name "foo" in
  let location_t = File default_location in
  let expression_variable = Location.wrap (Var.of_name "bar") in
  let type_expression : Ast_typed.type_expression =
    { type_content= T_variable (Var.of_name "foo");
      type_meta= None ;
      orig_var = None ;
      location= File default_location }
  in
  let value = Mini_c.D_none in
  error (`Spilling_corner_case ("foo", "bar")) ;
  [%expect
    {|
    foo
     corner case: bar
    Sorry, we don't have a proper error message for this error. Please report this use case so we can improve on this. |}] ;
  error (`Spilling_no_type_variable type_variable) ;
  [%expect{| Type "foo" not found (should not happen and be caught earlier). |}] ;
  error (`Spilling_unsupported_pattern_matching location_t) ;
  [%expect{|
    File "a dummy file name", line 20, character 5:

    Invalid pattern matching.@Tuple patterns are not (yet) supported. |}] ;
  error (`Spilling_unsupported_recursive_function expression_variable) ;
  [%expect{|
    Invalid recursive function "bar".
    A recursive function can only have one argument. |}] ;
  error (`Spilling_wrong_mini_c_value (type_expression, value)) ;
  [%expect{|
    Invalid type.
    Expected "foo",
    but got "None". |}] ;
  error (`Spilling_bad_decompile value) ;
  [%expect{| Cannot untranspile: None |}]

let%expect_test "stacking" =
  let open Mini_c in
  let error e = human_readable_error (`Main_stacking e) in
  error (`Stacking_corner_case ("foo", "bar")) ;
  [%expect
    {|
      Stacking corner case at foo : bar.
      Sorry, we don't have a proper error message for this error. Please report this use case so we can improve on this. |}] ;
  error (`Stacking_contract_entrypoint "xxx") ;
  [%expect {|contract entrypoint must be given as a literal string: xxx |}] ;
  error (`Stacking_bad_iterator C_INT) ;
  [%expect {|bad iterator: iter INT |}] ;
  error `Stacking_not_comparable_pair_struct ;
  [%expect
    {|Invalid comparable value. When using a tuple with more than 2 components, structure the tuple like this: "(a, (b, c))". |}]


let%expect_test "decompile_michelson" = () (* same as stacking *)

let%expect_test "decompile_mini_c" = ()  (* same as spilling *)

let%expect_test "decompile_typed" = () (* same as typer *)
