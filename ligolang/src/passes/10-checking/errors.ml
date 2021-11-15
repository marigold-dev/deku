open Simple_utils.Display

let stage = "typer"

type typer_error = [
  | `Typer_missing_funarg_annotation of Ast_typed.expression_variable
  | `Typer_michelson_comb_no_record of Location.t
  | `Typer_michelson_comb_no_variant of Location.t
  | `Typer_unbound_module_variable of Ast_typed.Environment.t * Ast_typed.module_variable * Location.t
  | `Typer_unbound_type_variable of Ast_typed.Environment.t * Ast_typed.type_variable * Location.t
  | `Typer_unbound_variable of Ast_typed.Environment.t * Ast_typed.expression_variable * Location.t
  | `Typer_match_missing_case of Ast_core.label list * Ast_core.label list * Location.t
  | `Typer_match_extra_case of Ast_core.label list * Ast_core.label list * Location.t
  | `Typer_unbound_constructor of Ast_typed.Environment.t * Ast_core.label * Location.t
  | `Typer_redundant_constructor of Ast_typed.Environment.t * Ast_core.label * Location.t
  | `Typer_type_constant_wrong_number_of_arguments of Ast_core.type_variable option * int * int * Location.t
  | `Typer_michelson_or_no_annotation of Ast_core.label * Location.t
  | `Typer_module_tracer of Ast_core.module_ * typer_error
  | `Typer_constant_declaration_tracer of Ast_core.expression_variable * Ast_core.expression * (Ast_typed.type_expression option) * typer_error
  | `Typer_match_error of Ast_typed.type_expression * Ast_typed.type_expression * Location.t
  | `Typer_needs_annotation of Ast_core.expression * string
  | `Typer_fvs_in_create_contract_lambda of Ast_core.expression * Ast_typed.expression_variable
  | `Typer_create_contract_lambda of Ast_core.constant' * Ast_core.expression
  | `Typer_should_be_a_function_type of Ast_typed.type_expression * Ast_core.expression
  | `Typer_bad_record_access of Ast_core.label * Ast_core.expression * Ast_typed.type_expression * Location.t
  | `Typer_expression_tracer of Ast_core.expression * typer_error
  | `Typer_record_access_tracer of Ast_typed.expression * typer_error
  | `Typer_assert_equal of Location.t * Ast_typed.type_expression * Ast_typed.type_expression
  | `Typer_corner_case of string
  | `Typer_bad_collect_loop of Ast_typed.type_expression * Location.t
  | `Typer_declaration_order_record of Location.t
  | `Typer_too_small_record of Location.t
  | `Typer_expected_record of Location.t * Ast_typed.type_expression
  | `Typer_expected_variant of Location.t * Ast_typed.type_expression
  | `Typer_wrong_param_number of Location.t * string * int * Ast_typed.type_expression list
  | `Typer_expected_function of Location.t * Ast_typed.type_expression
  | `Typer_expected_pair of Location.t * Ast_typed.type_expression
  | `Typer_expected_list of Location.t * Ast_typed.type_expression
  | `Typer_expected_set of Location.t * Ast_typed.type_expression
  | `Typer_expected_map of Location.t * Ast_typed.type_expression
  | `Typer_expected_big_map of Location.t * Ast_typed.type_expression
  | `Typer_expected_option of Location.t * Ast_typed.type_expression
  | `Typer_expected_nat of Location.t * Ast_typed.type_expression
  | `Typer_expected_bytes of Location.t * Ast_typed.type_expression
  | `Typer_expected_key of Location.t * Ast_typed.type_expression
  | `Typer_expected_signature of Location.t * Ast_typed.type_expression
  | `Typer_expected_contract of Location.t * Ast_typed.type_expression
  | `Typer_expected_typed_address of Location.t * Ast_typed.type_expression
  | `Typer_expected_address of Location.t * Ast_typed.type_expression
  | `Typer_expected_string of Location.t * Ast_typed.type_expression
  | `Typer_expected_key_hash of Location.t * Ast_typed.type_expression
  | `Typer_expected_mutez of Location.t * Ast_typed.type_expression
  | `Typer_expected_op_list of Location.t * Ast_typed.type_expression
  | `Typer_expected_int of Location.t * Ast_typed.type_expression
  | `Typer_expected_bool of Location.t * Ast_typed.type_expression
  | `Typer_expected_ticket of Location.t * Ast_typed.type_expression
  | `Typer_expected_sapling_transaction of Location.t * Ast_typed.type_expression
  | `Typer_expected_sapling_state of Location.t * Ast_typed.type_expression
  | `Typer_expected_ligo_code of Location.t * Ast_typed.type_expression
  | `Typer_expected_michelson_code of Location.t * Ast_typed.type_expression
  | `Typer_expected_unit of Location.t * Ast_typed.type_expression
  | `Typer_not_matching of Location.t * Ast_typed.type_expression * Ast_typed.type_expression
  | `Typer_not_annotated of Location.t
  | `Typer_contract_not_annotated of Location.t
  | `Typer_bad_substraction of Location.t
  | `Typer_wrong_size of Location.t * Ast_typed.type_expression
  | `Typer_wrong_neg of Location.t * Ast_typed.type_expression
  | `Typer_wrong_not of Location.t * Ast_typed.type_expression
  | `Typer_typeclass_error of Location.t * Ast_typed.type_expression list list * Ast_typed.type_expression list
  | `Typer_converter of Ast_typed.type_expression
  | `Typer_uncomparable_types of Location.t * Ast_typed.type_expression * Ast_typed.type_expression
  | `Typer_comparator_composed of Location.t * Ast_typed.type_expression
  | `Typer_constant_decl_tracer of Ast_core.expression_variable * Ast_core.expression * Ast_typed.type_expression option * typer_error
  | `Typer_unrecognized_type_constant of Ast_core.type_expression
  | `Typer_expected_ascription of Ast_core.expression
  | `Typer_different_types of Ast_typed.type_expression * Ast_typed.type_expression
  | `Typer_variant_redefined_error of Location.t
  | `Typer_record_redefined_error of Location.t
  | `Typer_constant_tag_number_of_arguments of string * Ast_typed.constant_tag * Ast_typed.constant_tag * int * int
  | `Typer_typeclass_not_a_rectangular_matrix
  | `Typer_internal_error of string * string
  | `Trace_debug of string * typer_error
  | `Typer_pattern_do_not_match of Location.t
  | `Typer_pattern_do_not_conform_type of Ast_core.type_expression Ast_core.pattern * Ast_typed.type_expression
  | `Typer_redundant_pattern of Location.t
  | `Typer_wrong_type_for_unit_pattern of Location.t * Ast_typed.type_expression
  | `Typer_poly_not_applied of Location.t
  | `Typer_wrong_generalizable of Location.t * Ast_core.type_variable
]

let wrong_type_for_unit_pattern l t = `Typer_wrong_type_for_unit_pattern (l,t)
let pattern_do_not_conform_type p t = `Typer_pattern_do_not_conform_type (p,t)
let pattern_do_not_match loc = `Typer_pattern_do_not_match loc
let missing_funarg_annotation v = `Typer_missing_funarg_annotation v
let variant_redefined_error (loc:Location.t) = `Typer_variant_redefined_error loc
let record_redefined_error (loc:Location.t) = `Typer_record_redefined_error loc
let michelson_comb_no_record (loc:Location.t) = `Typer_michelson_comb_no_record loc
let michelson_comb_no_variant (loc:Location.t) = `Typer_michelson_comb_no_variant loc
let unbound_module_variable (e:Ast_typed.Environment.t) (mv:Ast_typed.module_variable) (loc:Location.t) = `Typer_unbound_module_variable (e,mv,loc)
let unbound_type_variable (e:Ast_typed.Environment.t) (tv:Ast_typed.type_variable) (loc:Location.t) = `Typer_unbound_type_variable (e,tv,loc)
let unbound_variable (e:Ast_typed.Environment.t) (v:Ast_typed.expression_variable) (loc:Location.t) = `Typer_unbound_variable (e,v,loc)
let match_missing_case (m:Ast_core.label list) (v:Ast_core.label list) (loc:Location.t) = `Typer_match_missing_case (m, v, loc)
let match_extra_case (m:Ast_core.label list) (v:Ast_core.label list) (loc:Location.t) = `Typer_match_extra_case (m, v, loc)
let unbound_constructor (e:Ast_typed.Environment.t) (c:Ast_core.label) (loc:Location.t) = `Typer_unbound_constructor (e,c,loc)
let type_constant_wrong_number_of_arguments (op:Ast_core.type_variable option) (expected:int) (actual:int) loc = `Typer_type_constant_wrong_number_of_arguments (op,expected,actual,loc)
let redundant_constructor (e:Ast_typed.Environment.t) (c:Ast_core.label) (loc:Location.t) = `Typer_redundant_constructor (e,c,loc)
let michelson_or (c:Ast_core.label) (loc:Location.t) = `Typer_michelson_or_no_annotation (c,loc)
let module_error_tracer (p:Ast_core.module_) (err:typer_error) = `Typer_module_tracer (p,err)
let constant_declaration_error_tracer (name:Ast_core.expression_variable) (ae:Ast_core.expression) (expected: Ast_typed.type_expression option) (err:typer_error) =
  `Typer_constant_declaration_tracer (name,ae,expected,err)
let match_error ~(expected: Ast_typed.type_expression) ~(actual: Ast_typed.type_expression) (loc:Location.t) =
  `Typer_match_error (expected,actual,loc)
let needs_annotation (e:Ast_core.expression) (case:string) = `Typer_needs_annotation (e,case)
let fvs_in_create_contract_lambda (e:Ast_core.expression) (fvar:Ast_typed.expression_variable) = `Typer_fvs_in_create_contract_lambda (e,fvar)
let create_contract_lambda (cst : Ast_core.constant') (e : Ast_core.expression) = `Typer_create_contract_lambda (cst,e)
let type_error_approximate ~(actual: Ast_typed.type_expression) ~(expression:Ast_core.expression) =
  `Typer_should_be_a_function_type (actual,expression)
let bad_record_access (field:Ast_core.label) (ae:Ast_core.expression) (t:Ast_typed.type_expression) (loc:Location.t) =
  `Typer_bad_record_access (field,ae,t,loc)
let expression_tracer ae err = `Typer_expression_tracer (ae,err)
let record_access_tracer (e:Ast_typed.expression) (err:typer_error) = `Typer_record_access_tracer (e,err)
let assert_equal (loc:Location.t) (expected:Ast_typed.type_expression) (actual:Ast_typed.type_expression) = `Typer_assert_equal (loc,expected,actual)
let corner_case desc = `Typer_corner_case desc
let bad_collect_loop (t:Ast_typed.type_expression) (loc:Location.t) = `Typer_bad_collect_loop (t,loc)
let declaration_order_record (loc:Location.t) = `Typer_declaration_order_record loc
let too_small_record (loc:Location.t) = `Typer_too_small_record loc
let expected_record (loc:Location.t) (t:Ast_typed.type_expression) = `Typer_expected_record (loc,t)
let expected_variant (loc:Location.t) (t:Ast_typed.type_expression) = `Typer_expected_variant (loc,t)
let wrong_param_number (loc:Location.t) (name:string) (expected:int) (actual:Ast_typed.type_expression list) =
  `Typer_wrong_param_number (loc,name,expected,actual)
let bad_list_fold_tracer err = `Typer_bad_list_fold_tracer err
let bad_set_fold_tracer err = `Typer_bad_set_fold_tracer err
let bad_map_fold_tracer err = `Typer_bad_map_fold_tracer err
let expected_function (loc:Location.t) (t:Ast_typed.type_expression) = `Typer_expected_function (loc,t)
let expected_pair (loc:Location.t) (t:Ast_typed.type_expression) = `Typer_expected_pair (loc,t)
let expected_list (loc:Location.t) (t:Ast_typed.type_expression) = `Typer_expected_list (loc,t)
let expected_sapling_transaction (loc:Location.t) (t:Ast_typed.type_expression) = `Typer_expected_sapling_transaction (loc,t)
let expected_sapling_state (loc:Location.t) (t:Ast_typed.type_expression) = `Typer_expected_sapling_state (loc,t)
let expected_ligo_code (loc:Location.t) (t:Ast_typed.type_expression) = `Typer_expected_ligo_code (loc,t)
let expected_michelson_code (loc:Location.t) (t:Ast_typed.type_expression) = `Typer_expected_michelson_code (loc,t)
let expected_address (loc:Location.t) (t:Ast_typed.type_expression) = `Typer_expected_address (loc,t)
let expected_set (loc:Location.t) (t:Ast_typed.type_expression) = `Typer_expected_set (loc,t)
let expected_map (loc:Location.t) (t:Ast_typed.type_expression) = `Typer_expected_map (loc,t)
let expected_big_map (loc:Location.t) (t:Ast_typed.type_expression) = `Typer_expected_big_map (loc,t)
let expected_option (loc:Location.t) (t:Ast_typed.type_expression) = `Typer_expected_option (loc,t)
let expected_nat (loc:Location.t) (t:Ast_typed.type_expression) = `Typer_expected_nat (loc,t)
let expected_bytes (loc:Location.t) (t:Ast_typed.type_expression) = `Typer_expected_bytes (loc,t)
let expected_key (loc:Location.t) (t:Ast_typed.type_expression) = `Typer_expected_key (loc,t)
let expected_signature (loc:Location.t) (t:Ast_typed.type_expression) = `Typer_expected_signature (loc,t)
let expected_contract (loc:Location.t) (t:Ast_typed.type_expression) = `Typer_expected_contract (loc,t)
let expected_typed_address (loc:Location.t) (t:Ast_typed.type_expression) = `Typer_expected_typed_address (loc,t)
let expected_ticket (loc:Location.t) (t:Ast_typed.type_expression) = `Typer_expected_ticket (loc,t)
let expected_string (loc:Location.t) (t:Ast_typed.type_expression) = `Typer_expected_string (loc,t)
let expected_key_hash (loc:Location.t) (t:Ast_typed.type_expression) = `Typer_expected_key_hash (loc,t)
let expected_mutez (loc:Location.t) (t:Ast_typed.type_expression) = `Typer_expected_mutez (loc,t)
let expected_op_list (loc:Location.t) (t:Ast_typed.type_expression) = `Typer_expected_op_list (loc,t)
let expected_int (loc:Location.t) (t:Ast_typed.type_expression) = `Typer_expected_int (loc,t)
let expected_bool (loc:Location.t) (t:Ast_typed.type_expression) = `Typer_expected_bool (loc,t)
let expected_unit (loc:Location.t) (t:Ast_typed.type_expression) = `Typer_expected_unit (loc,t)
let expected_ascription (t:Ast_core.expression) = `Typer_expected_ascription t
let not_matching (loc:Location.t) (t1:Ast_typed.type_expression) (t2:Ast_typed.type_expression) = `Typer_not_matching (loc,t1,t2)
let not_annotated (loc: Location.t) = `Typer_not_annotated loc
let contract_not_annotated (loc: Location.t) = `Typer_contract_not_annotated loc
let bad_subtraction (loc:Location.t) = `Typer_bad_substraction loc
let wrong_size (loc:Location.t) (t:Ast_typed.type_expression) = `Typer_wrong_size (loc,t)
let wrong_neg (loc:Location.t) (t:Ast_typed.type_expression) = `Typer_wrong_neg (loc,t)
let wrong_not (loc:Location.t) (t:Ast_typed.type_expression) = `Typer_wrong_not (loc,t)
let typeclass_error (loc:Location.t) (exps:Ast_typed.type_expression list list) (acts:Ast_typed.type_expression list) =
  `Typer_typeclass_error (loc,exps,acts)
let wrong_converter (t:Ast_typed.type_expression) = `Typer_converter t
let uncomparable_types (loc:Location.t) (a:Ast_typed.type_expression) (b:Ast_typed.type_expression) =
  `Typer_uncomparable_types (loc,a,b)
let comparator_composed (loc:Location.t) (a:Ast_typed.type_expression) = `Typer_comparator_composed (loc,a)
let unrecognized_type_constant (e:Ast_core.type_expression) = `Typer_unrecognized_type_constant e
let poly_not_applied (loc:Location.t) = `Typer_poly_not_applied loc
let wrong_generalizable (loc:Location.t) (t:Ast_core.type_variable) = `Typer_wrong_generalizable (loc, t)

(* new typer errors *)
let constant_declaration_tracer (name: Ast_core.expression_variable) (ae:Ast_core.expression) (expected: Ast_typed.type_expression option) (err:typer_error) =
  `Typer_constant_decl_tracer (name,ae,expected,err)
let different_types a b = `Typer_different_types (a,b)
let different_constant_tag_number_of_arguments loc opa opb lena lenb = `Typer_constant_tag_number_of_arguments (loc, opa, opb, lena, lenb)
let typeclass_not_a_rectangular_matrix = `Typer_typeclass_not_a_rectangular_matrix
let internal_error (loc : string) (msg : string) : typer_error = `Typer_internal_error (loc, msg)
let could_not_remove = fun constraints -> `Typer_could_not_remove constraints
let trace_debug (msg : string) (err : typer_error) : typer_error = `Trace_debug (msg,err)
let redundant_pattern (x : Location.t) : typer_error = `Typer_redundant_pattern x


let rec error_ppformat : display_format:string display_format ->
  Format.formatter -> typer_error -> unit =
  fun ~display_format f a ->
  match display_format with
  | Human_readable | Dev -> (
    match a with
    | `Trace_debug (msg,err) ->
      (match display_format with
      | Human_readable -> Format.fprintf f "%a" (error_ppformat ~display_format) err
      | _ -> Format.fprintf f "%s\n%a" msg (error_ppformat ~display_format) err)
    | `Typer_wrong_type_for_unit_pattern (l,t) ->
      Format.fprintf f
        "@[<hv>%a@.Variant pattern argument is expected of type %a but is of type unit.@]"
          Snippet.pp l
          Ast_typed.PP.type_expression t
    | `Typer_pattern_do_not_match loc ->
      Format.fprintf f
        "@[<hv>%a@.Pattern do not match returned expression.@]"
          Snippet.pp loc
    | `Typer_missing_funarg_annotation v ->
      Format.fprintf f
        "@[<hv>%a@.Missing a type annotation for argument \"%a\".@]"
          Snippet.pp v.location
          Ast_typed.PP.expression_variable v
    | `Typer_michelson_comb_no_record loc ->
      Format.fprintf f
        "@[<hv>%a@.Invalid usage of type \"michelson_pair\".@.The \"michelson_pair\" type expects a record type as argument. @]"
        Snippet.pp loc
    | `Typer_michelson_comb_no_variant loc ->
      Format.fprintf f
        "@[<hv>%a@.Invalid usage of type \"michelson_or\".@.The \"michelson_or\" type expects a variant type as argument. @]"
        Snippet.pp loc
    | `Typer_unbound_module_variable (_env,mv,loc) ->
      Format.fprintf f
        "@[<hv>%a@.Module \"%a\" not found. @]"
        Snippet.pp loc
        Ast_typed.PP.module_variable mv
    | `Typer_unbound_type_variable (_env,tv,loc) ->
      Format.fprintf f
        "@[<hv>%a@.Type \"%a\" not found. @]"
        Snippet.pp loc
        Ast_typed.PP.type_variable tv
    | `Typer_unbound_variable (_env,v,loc) ->
      Format.fprintf f
        "@[<hv>%a@.Variable \"%a\" not found. @]"
        Snippet.pp loc
        Ast_typed.PP.expression_variable v
    | `Typer_match_missing_case (m, v, loc) ->
      let missing = List.fold_left ~f:(fun all o ->
        match List.find ~f:(fun f -> f = o) v with
        | Some _ -> all
        | None ->
          let (Label o) = o in
          o :: all
      ) ~init:[] m in
      let missing = String.concat ", " missing in
      Format.fprintf f
        "@[<hv>%a@.Pattern matching is not exhaustive.@.Cases that are missing: %s. @]"
        Snippet.pp loc
        missing
    | `Typer_match_extra_case (m, v,loc) ->
      let open Ast_core in
      let rec extra (processed: string list) (redundant: string list) (unknown: string list) = function
      | Label l :: remaining -> (
        match (List.find ~f:(fun f -> f = Label l) m)  with
        | Some _ -> (
          match (List.find ~f:(fun f -> f = l) processed) with
          | Some _ -> extra processed (l :: redundant) unknown remaining
          | None -> extra (l :: processed) redundant unknown remaining
        )
        | None -> extra processed redundant (l :: unknown) remaining)
      | [] -> (List.rev redundant, List.rev unknown)
      in
      let (redundant, unknown) = extra [] [] [] v in
      Format.fprintf f "@[<hv>%a@.Pattern matching over too many cases.@]"
        Snippet.pp loc;
      if List.length redundant > 0 then (
        let redundant = String.concat ", " redundant in
        Format.fprintf f
          "@[<hv>@.These case(s) are duplicate:@.%s@]"
          redundant
      );
      if List.length unknown > 0 then (
        let unknown = String.concat ", " unknown in
        Format.fprintf f
          "@[<hv>@.These case(s) don't belong to the variant:@.%s@]"
          unknown
      );
      Format.fprintf f
          "@[<hv>@.Please remove the extra cases. @]"
    | `Typer_unbound_constructor (_env,c,loc) ->
      Format.fprintf f
        "@[<hv>%a@.Constructor \"%a\" not found. @]"
        Snippet.pp loc
        Ast_core.PP.label c
    | `Typer_redundant_constructor (_env,c,loc) ->
      Format.fprintf f
        "@[<hv>%a@.Invalid variant.@.Constructor \"%a\" already exists as part of another variant. @]"
        Snippet.pp loc
        Ast_core.PP.label c
    | `Typer_type_constant_wrong_number_of_arguments (op_opt,e,a,loc) ->
      let aux : Format.formatter -> Ast_core.type_variable option -> unit =
        fun ppf operator_opt ->
          match operator_opt with
          | Some v -> Format.fprintf ppf " %a" Ast_core.PP.type_variable v
          | None -> ()
      in
      Format.fprintf f
        "@[<hv>%a@ Type%a takes the wrong number of arguments, expected: %i got: %i@]"
        Snippet.pp loc
        aux op_opt
        e a
    | `Typer_michelson_or_no_annotation (c,loc) ->
      Format.fprintf f
        "@[<hv>%a@.Incorrect usage of type \"michelson_or\".@.The contructor \"%a\" must be annotated with a variant type. @]"
        Snippet.pp loc
        Ast_core.PP.label c
    | `Typer_module_tracer (_module,err) ->
      Format.fprintf f
        "%a"
        (error_ppformat ~display_format) err
    | `Typer_constant_declaration_tracer (_,_,_,err) ->
      error_ppformat ~display_format f err
    | `Typer_match_error (expected,actual,loc) ->
      Format.fprintf f
        "@[<hv>%a@.Pattern matching over an expression of an incorrect type.@.Type \"%a\" was expected, but got type \"%a\". @]"
        Snippet.pp loc
        Ast_typed.PP.type_expression expected
        Ast_typed.PP.type_expression actual
    | `Typer_needs_annotation (exp,case) ->
      Format.fprintf f
        "@[<hv>%a@.Missing type annotation.@.'%s' needs to be annotated with a type.@]"
        Snippet.pp exp.location
        case
    | `Typer_fvs_in_create_contract_lambda (e,fvar) ->
      Format.fprintf f
        "@[<hv>%a@.Free variable '%a' is not allowed in CREATE_CONTRACT lambda@]"
        Snippet.pp e.location
        Ast_typed.PP.expression_variable fvar
    | `Typer_create_contract_lambda (_cst,e) ->
      Format.fprintf f
        "@[<hv>%a@.Invalid usage of Tezos.create_contract.@.The first argument must be an inline function. @]"
        Snippet.pp e.location
    | `Typer_should_be_a_function_type (actual,e) ->
      Format.fprintf f
        "@[<hv>%a@.Invalid type.@.Expected a function type, but got \"%a\". @]"
        Snippet.pp e.location
        Ast_typed.PP.type_expression actual
    | `Typer_bad_record_access (field,ae,_t,loc) ->
      Format.fprintf f
        "@[<hv>%a@.Invalid record field \"%a\" in record \"%a\". @]"
        Snippet.pp loc
        Ast_core.PP.label field
        Ast_core.PP.expression ae
    | `Typer_corner_case desc ->
      Format.fprintf f
        "@[<hv>A type system corner case occurred:@.%s@]"
        desc
    | `Typer_bad_collect_loop (t,loc) ->
      Format.fprintf f
        "@[<hv>%a@.Bounded loop over a value with an incorrect type.@.Expected a value with type: \"list\", \"set\" or \"map\", but got a value of type \"%a\". @]"
        Snippet.pp loc
        Ast_typed.PP.type_expression t
    | `Typer_too_small_record loc ->
      Format.fprintf f
        "@[<hv>%a@.Incorrect argument provided to Layout.convert_to_(left|right)_comb.@.The record must have at least two elements. @]"
        Snippet.pp loc
    | `Typer_expression_tracer (_,err) -> error_ppformat ~display_format f err
    | `Typer_record_access_tracer (_,err) -> error_ppformat ~display_format f err
    | `Typer_not_annotated l ->
      Format.fprintf f "@[<hv>%a@.Can't infer the type of this value, please add a type annotation.@]"
        Snippet.pp l
    | `Typer_contract_not_annotated l ->
      Format.fprintf f "@[<hv>%a@.Can't infer the complete type of this value, please add a type annotation.\
        @.The value has type 'a contract option, here 'a can't be inferred@]"
        Snippet.pp l
    | `Typer_bad_substraction loc ->
      Format.fprintf f "@[<hv>%a@.Invalid subtraction.\
        @.The following forms of subtractions are possible:\
        @.  * timestamp - int = timestamp\
        @.  * timestamp - timestamp = int\
        @.  * int/nat - int/nat = int\
        @.  * mutez/tez - mutez/tez = mutez.@]"
        Snippet.pp loc
    | `Typer_wrong_size (loc,_t) ->
      Format.fprintf f
        "@[<hv>%a@.Incorrect value applied.@.A value with one of the following types is expected: map, list, string, byte or set. @]"
        Snippet.pp loc
    | `Typer_wrong_neg (loc,t) ->
      Format.fprintf f
        "@[<hv>%a@.Invalid value used for negation.@.Expected a value of type nat or int, but got %a. @]"
        Snippet.pp loc
        Ast_typed.PP.type_expression t
    | `Typer_wrong_not (loc,t) ->
      Format.fprintf f
        "@[<hv>%a@.Invalid value used for not operation.@.Expected a value of type Boolean, nat or int, but got %a. @]"
        Snippet.pp loc
        Ast_typed.PP.type_expression t
    | `Typer_converter t ->
      Format.fprintf f
        "@[<hv>%a@.Invalid usage of a Michelson converter.@.Converters can only be used on records or variants, but got %a. @]"
        Snippet.pp t.location
        Ast_typed.PP.type_expression t
    | `Typer_comparator_composed (loc,_a) ->
      Format.fprintf f
        "@[<hv>%a@.Invalid arguments.@.Only composed types of not more than two element are allowed to be compared. @]"
        Snippet.pp loc
    | `Typer_constant_decl_tracer (_name,_ae,_expected,err) ->
      Format.fprintf f
        "%a" (error_ppformat ~display_format) err
    | `Typer_unrecognized_type_constant e ->
      Format.fprintf f
        "@[<hv>%a@.Unrecognized type constant %a. @]"
        Snippet.pp e.location
        Ast_core.PP.type_expression e
    | `Typer_expected_ascription t ->
      Format.fprintf f
        "@[<hv>%a@.Invalid argument.@.At this point a block of code is expected, but got \"%a\". @]"
        Snippet.pp t.location
        Ast_core.PP.expression t
    | `Typer_different_types (a, b) ->
      Format.fprintf f
        "@[<hv>%a@.This expression has type %a, but an expression was expected of type %a.@]"
        Snippet.pp b.location
        Ast_typed.PP.type_expression b
        Ast_typed.PP.type_expression a
    | `Typer_assert_equal (loc, expected,actual) ->
      Format.fprintf f
        "@[<hv>%a@.Invalid type(s).@.Expected: \"%a\", but got: \"%a\". @]"
        Snippet.pp loc
        Ast_typed.PP.type_expression expected
        Ast_typed.PP.type_expression actual
    | `Typer_expected_record (loc,t) ->
      Format.fprintf f
        "@[<hv>%a@.Invalid argument.@.Expected a record, but got an argument of type \"%a\". @]"
        Snippet.pp loc
        Ast_typed.PP.type_expression t
    | `Typer_expected_variant (loc,t) ->
      Format.fprintf f
        "@[<hv>%a@.Invalid argument.@.Expected a variant, but got an argument of type \"%a\". @]"
        Snippet.pp loc
        Ast_typed.PP.type_expression t
    | `Typer_wrong_param_number (loc,name,expected,actual) ->
      Format.fprintf f
        "@[<hv>%a@.Function \"%s\" called with wrong number of arguments.@.Expected %d arguments, got %d arguments. @]"
        Snippet.pp loc
        name
        expected (List.length actual)
    | `Typer_expected_ligo_code (loc,t) ->
      Format.fprintf f
        "@[<hv>%a@.Invalid argument.@.Expected raw ligo code, but got an argument of type \"%a\". @]"
        Snippet.pp loc
        Ast_typed.PP.type_expression t
    | `Typer_expected_michelson_code (loc,t) ->
      Format.fprintf f
        "@[<hv>%a@.Invalid argument.@.Expected raw michelson code, but got an argument of type \"%a\". @]"
        Snippet.pp loc
        Ast_typed.PP.type_expression t
    | `Typer_expected_sapling_state (loc,t) ->
      Format.fprintf f
        "@[<hv>%a@.Invalid argument.@.Expected sapling_state, but got an argument of type \"%a\". @]"
        Snippet.pp loc
        Ast_typed.PP.type_expression t
    | `Typer_expected_sapling_transaction (loc,t) ->
      Format.fprintf f
        "@[<hv>%a@.Invalid argument.@.Expected sapling_transaction, but got an argument of type \"%a\". @]"
        Snippet.pp loc
        Ast_typed.PP.type_expression t
    | `Typer_expected_function (loc,e) ->
      Format.fprintf f
        "@[<hv>%a@.Invalid argument.@.Expected a function, but got an argument of type \"%a\". @]"
        Snippet.pp loc
        Ast_typed.PP.type_expression e
    | `Typer_expected_pair (loc,e) ->
      Format.fprintf f
        "@[<hv>%a@.Incorrect argument.@.Expected a pair, but got an argument of type \"%a\". @]"
        Snippet.pp loc
        Ast_typed.PP.type_expression e
    | `Typer_expected_list (loc,e) ->
      Format.fprintf f
        "@[<hv>%a@.Incorrect argument.@.Expected a list, but got an argument of type \"%a\". @]"
        Snippet.pp loc
        Ast_typed.PP.type_expression e
    | `Typer_expected_set (loc,e) ->
      Format.fprintf f
        "@[<hv>%a@.Incorrect argument.@.Expected a set, but got an argument of type \"%a\". @]"
        Snippet.pp loc
        Ast_typed.PP.type_expression e
    | `Typer_expected_map (loc,e) ->
      Format.fprintf f
        "@[<hv>%a@.Incorrect argument.@.Expected a map, but got an argument of type \"%a\". @]"
        Snippet.pp loc
        Ast_typed.PP.type_expression e
    | `Typer_expected_big_map (loc,e) ->
      Format.fprintf f
        "@[<hv>%a@.Incorrect argument.@.Expected a big_map, but got an argument of type \"%a\". @]"
        Snippet.pp loc
        Ast_typed.PP.type_expression e
    | `Typer_expected_option (loc,e) ->
      Format.fprintf f
        "@[<hv>%a@.Incorrect argument.@.Expected an option, but got an argument of type \"%a\". @]"
        Snippet.pp loc
        Ast_typed.PP.type_expression e
    | `Typer_expected_nat (loc,t) ->
      Format.fprintf f
        "@[<hv>%a@.Incorrect argument.@.Expected a nat, but got an argument of type \"%a\". @]"
        Snippet.pp loc
        Ast_typed.PP.type_expression t
    | `Typer_expected_bytes (loc,t) ->
      Format.fprintf f
        "@[<hv>%a@.Incorrect argument.@.Expected bytes, but got an argument of type \"%a\". @]"
        Snippet.pp loc
        Ast_typed.PP.type_expression t
    | `Typer_expected_key (loc,t) ->
      Format.fprintf f
        "@[<hv>%a@.Incorrect argument.@.Expected a key, but got an argument of type \"%a\". @]"
        Snippet.pp loc
        Ast_typed.PP.type_expression t
    | `Typer_expected_signature (loc,t) ->
      Format.fprintf f
        "@[<hv>%a@.Incorrect argument.@.Expected a signature, but got an argument of type \"%a\". @]"
        Snippet.pp loc
        Ast_typed.PP.type_expression t
    | `Typer_expected_contract (loc,t) ->
      Format.fprintf f
        "@[<hv>%a@.Incorrect argument.@.Expected a contract, but got an argument of type \"%a\". @]"
        Snippet.pp loc
        Ast_typed.PP.type_expression t
    | `Typer_expected_typed_address (loc,t) ->
      Format.fprintf f
        "@[<hv>%a@.Incorrect argument.@.Expected a typed address, but got an argument of type \"%a\". @]"
        Snippet.pp loc
        Ast_typed.PP.type_expression t
    | `Typer_expected_address (loc,t) ->
      Format.fprintf f
        "@[<hv>%a@.Incorrect argument.@.Expected a address, but got an argument of type \"%a\". @]"
        Snippet.pp loc
        Ast_typed.PP.type_expression t
    | `Typer_expected_ticket (loc,t) ->
        Format.fprintf f
          "@[<hv>%a@.Incorrect argument.@.Expected a ticket, but got an argument of type \"%a\". @]"
          Snippet.pp loc
          Ast_typed.PP.type_expression t
    | `Typer_expected_string (loc,t) ->
      Format.fprintf f
        "@[<hv>%a@.Incorrect argument.@.Expected a string, but got an argument of type \"%a\". @]"
        Snippet.pp loc
        Ast_typed.PP.type_expression t
    | `Typer_expected_key_hash (loc,t) ->
      Format.fprintf f
        "@[<hv>%a@.Incorrect argument.@.Expected a key hash, but got an argument of type \"%a\". @]"
        Snippet.pp loc
        Ast_typed.PP.type_expression t
    | `Typer_expected_mutez (loc,t) ->
      Format.fprintf f
        "@[<hv>%a@.Incorrect argument.@.Expected a mutez, but got an argument of type \"%a\". @]"
        Snippet.pp loc
        Ast_typed.PP.type_expression t
    | `Typer_expected_op_list (loc,t) ->
      Format.fprintf f
        "@[<hv>%a@.Incorrect argument.@.Expected a list of operations, but got an argument of type \"%a\". @]"
        Snippet.pp loc
        Ast_typed.PP.type_expression t
    | `Typer_expected_int (loc,t) ->
      Format.fprintf f
        "@[<hv>%a@.Incorrect argument.@.Expected an int, but got an argument of type \"%a\". @]"
        Snippet.pp loc
        Ast_typed.PP.type_expression t
    | `Typer_expected_unit (loc,t) ->
      Format.fprintf f
        "@[<hv>%a@.Incorrect argument.@.Expected unit, but got an argument of type \"%a\". @]"
        Snippet.pp loc
        Ast_typed.PP.type_expression t
    | `Typer_expected_bool (loc,t) ->
      Format.fprintf f
        "@[<hv>%a@.Incorrect argument.@.Expected a boolean, but got an argument of type \"%a\". @]"
        Snippet.pp loc
        Ast_typed.PP.type_expression t
    | `Typer_not_matching (loc,t1,t2) ->
      Format.fprintf f
        "@[<hv>%a@.These types are not matching:@. - %a@. - %a@]"
        Snippet.pp loc
        Ast_typed.PP.type_expression t1
        Ast_typed.PP.type_expression t2
    | `Typer_uncomparable_types (loc,a,b) ->
      Format.fprintf f
        "@[<hv>%a@.Invalid arguments.@.These types cannot be compared: \"%a\" and \"%a\". @]"
        Snippet.pp loc
        Ast_typed.PP.type_expression a
        Ast_typed.PP.type_expression b
    | `Typer_typeclass_error (loc,exps,acts) ->
      let open Simple_utils.PP_helpers in
      let printl printer ppf args =
        Format.fprintf ppf "(%a)" (list_sep printer (const ", ")) args in
        Format.fprintf f
        "@[<hv>%a@.Invalid arguments.@.Expected an argument of type %a, but got an argument of type %a. @]"
        Snippet.pp loc
        (list_sep (printl Ast_typed.PP.type_expression) (const " or ")) exps
        (list_sep Ast_typed.PP.type_expression (const ", ")) acts
    | `Typer_declaration_order_record loc ->
      Format.fprintf f
        "@[<hv>%a@.Incorrect argument provided to Layout.convert_to_(left|right)_comb.@.The given argument must be annotated with the type of the value. @]"
        Snippet.pp loc
    | `Typer_variant_redefined_error loc ->
      Format.fprintf f
        "@[<hv>%a@.Redefined variant. @]"
        Snippet.pp loc
    | `Typer_record_redefined_error loc ->
      Format.fprintf f
        "@[<hv>%a@.Redefined record. @]"
        Snippet.pp loc
    | `Typer_constant_tag_number_of_arguments (loc, opa, _opb, lena, lenb) ->
      Format.fprintf f
        "@[<hv> different number of arguments to type constructors.@ \
        Expected these two n-ary type constructors to be the same, but they have different number\
        of arguments (both use the %s type constructor, but they have %d and %d arguments, respectively)@ \
        Thrown by compiler at %s@]"
        (Format.asprintf "%a" Ast_typed.PP.constant_tag opa) lena lenb loc
    | `Typer_typeclass_not_a_rectangular_matrix ->
      Format.fprintf f "@[<hv>internal error: typeclass is not represented as a rectangular matrix with one column per argument@]"
    | `Typer_internal_error (loc, msg) -> Format.fprintf f "internal error at %s: %s" loc msg
    | `Typer_pattern_do_not_conform_type (p,t) ->
      let pf ppf value =
        match p.location with
        | Virtual _ ->  Format.fprintf ppf "%a " (Stage_common.PP.match_pattern Ast_core.PP.type_expression) value
        | File _ -> ()
      in
      Format.fprintf f
        "@[<hv>%a@.Pattern %ado not conform type %a @]"
        Snippet.pp p.location pf p Ast_typed.PP.type_expression t
    | `Typer_redundant_pattern loc ->
      Format.fprintf f
        "@[<hv>%a@.Redundant pattern matching@]"
        Snippet.pp loc
    | `Typer_poly_not_applied loc ->
      Format.fprintf f
        "@[<hv>%a@.Polymorphic value is not applied enough@]"
        Snippet.pp loc
    | `Typer_wrong_generalizable (loc, t) ->
      Format.fprintf f
        "@[<hv>%a@.Invalid type name: %a is a generalizable variable@]"
        Snippet.pp loc Ast_core.PP.type_variable t
  )
let rec error_jsonformat : typer_error -> Yojson.Safe.t = fun a ->
  let json_error ~stage ~content =
    `Assoc [
      ("status", `String "error") ;
      ("stage", `String stage) ;
      ("content",  content )]
  in
  match a with
  | `Trace_debug (msg,err) ->
    let content = `Assoc [
      ("message", `String msg );
      ("children", error_jsonformat err); ] in
    json_error ~stage ~content
  | `Typer_wrong_type_for_unit_pattern (l,t) ->
    let message = "Variant pattern argument is unit" in
    let content = `Assoc [
      ("message", `String message );
      ("expected", Ast_typed.Yojson.type_expression t);
      ("location", Location.to_yojson l);
    ] in
    json_error ~stage ~content
  | `Typer_redundant_pattern loc ->
    let message = "Redundant pattern matching" in
    let content = `Assoc [
      ("message", `String message );
      ("location", Location.to_yojson loc);
    ] in
    json_error ~stage ~content
  | `Typer_pattern_do_not_match loc ->
    let message = Format.asprintf "Pattern do not match returned expression" in
    let content = `Assoc [
      ("message", `String message );
      ("location", Location.to_yojson loc); ] in
    json_error ~stage ~content
  | `Typer_missing_funarg_annotation v ->
    let message = Format.asprintf "Missing type annotation for argument" in
    let content = `Assoc [
      ("value", Stage_common.Types.expression_variable_to_yojson v );
      ("message", `String message );
      ("location", Location.to_yojson v.location); ] in
    json_error ~stage ~content
  | `Typer_michelson_comb_no_record loc ->
    let message = `String "michelson pair comb can only be used on a record type" in
    let loc = Format.asprintf "%a" Location.pp loc in
    let content = `Assoc [
      ("message", message);
      ("location", `String loc)
    ] in
    json_error ~stage ~content
  | `Typer_michelson_comb_no_variant loc ->
    let message = `String "michelson or comb can only be used on a variant type" in
    let loc = Format.asprintf "%a" Location.pp loc in
    let content = `Assoc [
      ("message", message);
      ("location", `String loc)
    ] in
    json_error ~stage ~content
  | `Typer_unbound_module_variable (env,mv,loc) ->
    let message = `String "unbound module" in
    let loc = Format.asprintf "%a" Location.pp loc in
    let value = Format.asprintf "%a" Ast_typed.PP.module_variable mv in
    let env = Format.asprintf "%a" Ast_typed.Environment.PP.environment env in
    let content = `Assoc [
      ("message", message);
      ("location", `String loc);
      ("value", `String value);
      ("env", `String env);
    ] in
    json_error ~stage ~content
  | `Typer_unbound_type_variable (env,tv,loc) ->
    let message = `String "unbound type variable" in
    let loc = Format.asprintf "%a" Location.pp loc in
    let value = Format.asprintf "%a" Ast_typed.PP.type_variable tv in
    let env = Format.asprintf "%a" Ast_typed.Environment.PP.environment env in
    let content = `Assoc [
      ("message", message);
      ("location", `String loc);
      ("value", `String value);
      ("env", `String env);
    ] in
    json_error ~stage ~content
  | `Typer_unbound_variable (env,v,loc) ->
    let message = `String "unbound type variable" in
    let loc = Format.asprintf "%a" Location.pp loc in
    let value = Format.asprintf "%a" Ast_typed.PP.expression_variable v in
    let env = Format.asprintf "%a" Ast_typed.Environment.PP.environment env in
    let content = `Assoc [
      ("message", message);
      ("location", `String loc);
      ("value", `String value);
      ("env", `String env);
    ] in
    json_error ~stage ~content
  | `Typer_match_missing_case (m, v, loc) ->
    let missing = List.fold_left ~f:(fun all o ->
      match List.find ~f:(fun f -> f = o) v with
      | Some _ -> all
      | None ->
        let (Label o) = o in
        `String o :: all
    ) ~init:[] m in
    let message = `String "Missing match case" in
    let loc = `String (Format.asprintf "%a" Location.pp loc) in
    let content = `Assoc [
      ("message", message);
      ("location", loc);
      ("value", `List missing);
    ] in
    json_error ~stage ~content
  | `Typer_match_extra_case (v, m,loc) ->
    let open Ast_core in
    let rec extra processed redundant unknown = function
    | Label l :: remaining -> (
      match (List.find ~f:(fun f -> f = Label l) m)  with
      | Some _ -> (
        match (List.find ~f:(fun f -> f = l) processed) with
        | Some _ -> extra processed (`String l :: redundant) unknown remaining
        | None -> extra (l :: processed) redundant unknown remaining
      )
      | None -> extra processed redundant (`String l :: unknown) remaining)
    | [] -> (List.rev redundant, List.rev unknown)
    in
    let (redundant, unknown) = extra [] [] [] v in
    let message = `String "Redundant case in match cases" in
    let loc = `String (Format.asprintf "%a" Location.pp loc) in
    let content = `Assoc [
      ("message", message);
      ("location", loc);
      ("redundant", `List redundant);
      ("unknown", `List unknown)
    ] in
    json_error ~stage ~content
  | `Typer_unbound_constructor (env,c,loc) ->
    let message = `String "unbound type variable" in
    let loc = Format.asprintf "%a" Location.pp loc in
    let value = Format.asprintf "%a" Ast_core.PP.label c in
    let env = Format.asprintf "%a" Ast_typed.Environment.PP.environment env in
    let content = `Assoc [
      ("message", message);
      ("location", `String loc);
      ("value", `String value);
      ("env", `String env);
    ] in
    json_error ~stage ~content
  | `Typer_redundant_constructor (env,c,loc) ->
    let message = `String "redundant constructor" in
    let loc = Format.asprintf "%a" Location.pp loc in
    let value = Format.asprintf "%a" Ast_core.PP.label c in
    let env = Format.asprintf "%a" Ast_typed.Environment.PP.environment env in
    let content = `Assoc [
      ("message", message);
      ("location", `String loc);
      ("value", `String value);
      ("env", `String env);
    ] in
    json_error ~stage ~content
  | `Typer_type_constant_wrong_number_of_arguments (op, e, a, loc) ->
    let message = `String "Wrong number of arguments for type constant" in
    let loc = Format.asprintf "%a" Location.pp loc in
    let op = Ast_core.Yojson.option Var.to_yojson op in
    let content = `Assoc [
      ("message", message);
      ("location", `String loc);
      ("type_constant", op);
      ("expected", `Int e);
      ("actuel", `Int a);
    ] in
    json_error ~stage ~content
  | `Typer_michelson_or_no_annotation (c,loc) ->
    let message = `String "michelson_or must be annotated with a sum type" in
    let loc = Format.asprintf "%a" Location.pp loc in
    let value = Format.asprintf "%a" Ast_core.PP.label c in
    let content = `Assoc [
      ("message", message);
      ("location", `String loc);
      ("value", `String value);
    ] in
    json_error ~stage ~content
  | `Typer_module_tracer (p,err) ->
    let message = `String "Typing module" in
    let over = List.fold_left ~f:(fun a (p:Ast_core.declaration Location.wrap) -> match p.location with File reg -> Region.cover a reg | Virtual _ -> a) ~init:Region.ghost p in
    let loc = `String (Format.asprintf "%a" Location.pp_lift over) in
    let content = `Assoc [
      ("message", message);
      ("location", loc);
      ("children", error_jsonformat err);
    ] in
    json_error ~stage ~content
  | `Typer_constant_declaration_tracer (name,ae,Some t,err) ->
    let message = `String "Typing constant declaration" in
    let value = `String (Format.asprintf "%a" Ast_core.PP.expression ae) in
    let loc = `String (Format.asprintf "%a" Location.pp name.location) in
    let name = `String (Format.asprintf "%a" Ast_core.PP.expression_variable name) in
    let expected = `String (Format.asprintf "%a" Ast_typed.PP.type_expression t) in
    let content = `Assoc [
      ("message", message);
      ("location", loc);
      ("name", name);
      ("value", value);
      ("expected", expected);
      ("children", error_jsonformat err);
    ] in
    json_error ~stage ~content
  | `Typer_constant_declaration_tracer (name,ae,None,err) ->
    let message = `String "Typing constant declaration" in
    let loc = Location.to_yojson ae.location in
    let name = `String (Format.asprintf "%a" Ast_core.PP.expression_variable name) in
    let content = `Assoc [
      ("message", message);
      ("location", loc);
      ("name", name);
      ("children", error_jsonformat err);
    ] in
    json_error ~stage ~content
  | `Typer_match_error (expected,actual,loc) ->
    let message = `String "matching over an expression of the wrong type" in
    let loc = Location.to_yojson loc in
    let expected = Ast_typed.Yojson.type_expression expected in
    let actual = Ast_typed.Yojson.type_expression actual in
    let content = `Assoc [
      ("message", message);
      ("location", loc);
      ("actual", actual);
      ("expected", expected);
    ] in
    json_error ~stage ~content
  | `Typer_needs_annotation (exp,case) ->
    let message = `String "This expression needs to be annotated with its type" in
    let loc = `String (Format.asprintf "%a" Location.pp exp.location) in
    let exp = `String (Format.asprintf "%a" Ast_core.PP.expression exp) in
    let content = `Assoc [
      ("message", message);
      ("location", loc);
      ("expression", exp);
      ("case", `String case);
    ] in
    json_error ~stage ~content
  | `Typer_fvs_in_create_contract_lambda (e,fvar) ->
    let message = `String "Free variables are not allowed in CREATE_CONTRACT lambdas" in
    let loc = `String (Format.asprintf "%a" Location.pp e.location) in
    let expression = `String (Format.asprintf "%a" Ast_core.PP.expression e) in
    let variable = `String (Format.asprintf "%a" Ast_typed.PP.expression_variable fvar) in
    let content = `Assoc [
      ("message", message);
      ("location", loc);
      ("expression", expression);
      ("variable", variable);
    ] in
    json_error ~stage ~content
  | `Typer_create_contract_lambda (cst,e) ->
    let message = `String (Format.asprintf "First argument of %a must be inlined using a lambda" Ast_core.PP.constant' cst) in
    let loc = `String (Format.asprintf "%a" Location.pp e.location) in
    let expression = `String (Format.asprintf "%a" Ast_core.PP.expression e) in
    let content = `Assoc [
      ("message", message);
      ("location", loc);
      ("expression", expression);
    ] in
    json_error ~stage ~content
  | `Typer_should_be_a_function_type (actual,e) ->
    let message = `String "expected a function type" in
    let loc = `String (Format.asprintf "%a" Location.pp e.location) in
    let expression = `String (Format.asprintf "%a" Ast_core.PP.expression e) in
    let actual = `String (Format.asprintf "%a" Ast_typed.PP.type_expression actual) in
    let content = `Assoc [
      ("message", message);
      ("location", loc);
      ("expression", expression);
      ("actual", actual);
    ] in
    json_error ~stage ~content
  | `Typer_bad_record_access (field,ae,t,loc) ->
    let message = `String "invalid record field" in
    let field = `String (Format.asprintf "%a" Ast_core.PP.label field) in
    let value = `String (Format.asprintf "%a" Ast_core.PP.expression ae) in
    let value_type = `String (Format.asprintf "%a" Ast_typed.PP.type_expression t) in
    let loc = `String (Format.asprintf "%a" Location.pp loc) in
    let content = `Assoc [
      ("message", message); ("location", loc);
      ("value", value); ("value_type", value_type);
      ("field", field);
    ] in
    json_error ~stage ~content
  | `Typer_expression_tracer (e,err) ->
    let expression = `String (Format.asprintf "%a" Ast_core.PP.expression e) in
    let loc = `String (Format.asprintf "%a" Location.pp e.location) in
    let content = `Assoc [
      ("location", loc);
      ("expression", expression);
      ("children", error_jsonformat err);
    ] in
    json_error ~stage ~content
  | `Typer_record_access_tracer (e,err) ->
    let message = `String "invalid record access" in
    let expression = `String (Format.asprintf "%a" Ast_typed.PP.expression e) in
    let loc = `String (Format.asprintf "%a" Location.pp e.location) in
    let content = `Assoc [
      ("message", message);
      ("location", loc);
      ("expression", expression);
      ("children", error_jsonformat err);
    ] in
    json_error ~stage ~content
  | `Typer_assert_equal (loc,expected,actual) ->
    let message = `String "bad types" in
    let expected = `String (Format.asprintf "%a" Ast_typed.PP.type_expression expected) in
    let actual = `String (Format.asprintf "%a" Ast_typed.PP.type_expression actual) in
    let content = `Assoc [
      ("location", Location.to_yojson loc);
      ("message" , message);
      ("expected", expected);
      ("actual"  , actual);
    ] in
    json_error ~stage ~content
  | `Typer_corner_case desc ->
    let message = `String desc in
    let content = `Assoc [
      ("message", message);
    ] in
    json_error ~stage ~content
  | `Typer_bad_collect_loop (t,loc) ->
    let message = `String "Loops over collections expect lists, sets or maps" in
    let actual = `String (Format.asprintf "%a" Ast_typed.PP.type_expression t) in
    let loc = `String (Format.asprintf "%a" Location.pp loc) in
    let content = `Assoc [
      ("message", message);
      ("location", loc);
      ("actual", actual);
    ] in
    json_error ~stage ~content
  | `Typer_declaration_order_record loc ->
    let message = `String "can't retrieve type declaration order in the converted record, you need to annotate it" in
    let loc = `String (Format.asprintf "%a" Location.pp loc) in
    let content = `Assoc [
      ("message", message);
      ("location", loc);
    ] in
    json_error ~stage ~content
  | `Typer_too_small_record loc ->
    let message = `String "converted record must have at least two elements" in
    let loc = `String (Format.asprintf "%a" Location.pp loc) in
    let content = `Assoc [
      ("message", message);
      ("location", loc);
    ] in
    json_error ~stage ~content
  | `Typer_expected_record (loc,t) ->
    let message = `String "expected a record" in
    let value = `String (Format.asprintf "%a" Ast_typed.PP.type_expression t) in
    let content = `Assoc [
      ("location", Location.to_yojson loc);
      ("message", message);
      ("value", value);
    ] in
    json_error ~stage ~content
  | `Typer_expected_variant (loc,t) ->
    let message = `String "expected a record" in
    let value = `String (Format.asprintf "%a" Ast_typed.PP.type_expression t) in
    let content = `Assoc [
      ("message", message);
      ("location", Location.to_yojson loc);
      ("value", value);
    ] in
    json_error ~stage ~content
  | `Typer_expected_unit (loc,t) ->
    let message = `String "expected unit" in
    let value = `String (Format.asprintf "%a" Ast_typed.PP.type_expression t) in
    let content = `Assoc [
      ("message", message);
      ("location", Location.to_yojson loc);
      ("value", value);
    ] in
    json_error ~stage ~content
  | `Typer_expected_nat (loc,t) ->
    let message = `String "expected a nat" in
    let value = `String (Format.asprintf "%a" Ast_typed.PP.type_expression t) in
    let content = `Assoc [
      ("message", message);
      ("location", Location.to_yojson loc);
      ("value", value);
    ] in
    json_error ~stage ~content
  | `Typer_expected_bytes (loc,t) ->
    let message = `String "expected bytes" in
    let value = `String (Format.asprintf "%a" Ast_typed.PP.type_expression t) in
    let content = `Assoc [
      ("message", message);
      ("location", Location.to_yojson loc);
      ("value", value);
    ] in
    json_error ~stage ~content
  | `Typer_expected_key (loc,t) ->
    let message = `String "expected key" in
    let value = `String (Format.asprintf "%a" Ast_typed.PP.type_expression t) in
    let content = `Assoc [
      ("message", message);
      ("location", Location.to_yojson loc);
      ("value", value);
    ] in
    json_error ~stage ~content
  | `Typer_expected_signature (loc,t) ->
    let message = `String "expected signature" in
    let value = `String (Format.asprintf "%a" Ast_typed.PP.type_expression t) in
    let content = `Assoc [
      ("message", message);
      ("location", Location.to_yojson loc);
      ("value", value);
    ] in
    json_error ~stage ~content
  | `Typer_expected_contract (loc,t) ->
    let message = `String "expected contract" in
    let value = `String (Format.asprintf "%a" Ast_typed.PP.type_expression t) in
    let content = `Assoc [
      ("message", message);
      ("location", Location.to_yojson loc);
      ("value", value);
    ] in
    json_error ~stage ~content
  | `Typer_expected_typed_address (loc,t) ->
    let message = `String "expected typed address" in
    let value = `String (Format.asprintf "%a" Ast_typed.PP.type_expression t) in
    let content = `Assoc [
      ("message", message);
      ("location", Location.to_yojson loc);
      ("value", value);
    ] in
    json_error ~stage ~content
  | `Typer_expected_address (loc,t) ->
    let message = `String "expected address" in
    let value = `String (Format.asprintf "%a" Ast_typed.PP.type_expression t) in
    let content = `Assoc [
      ("message", message);
      ("location", Location.to_yojson loc);
      ("value", value);
    ] in
    json_error ~stage ~content
  | `Typer_expected_ticket (loc,t) ->
    let message = `String "expected ticket" in
    let value = `String (Format.asprintf "%a" Ast_typed.PP.type_expression t) in
    let content = `Assoc [
      ("message", message);
      ("location", Location.to_yojson loc);
      ("value", value);
    ] in
    json_error ~stage ~content
  | `Typer_expected_string (loc,t) ->
    let message = `String "expected string" in
    let content = `Assoc [
      ("message", message);
      ("location", Location.to_yojson loc);
      ("value", Ast_typed.Yojson.type_expression t);
    ] in
    json_error ~stage ~content
  | `Typer_expected_key_hash (loc,t) ->
    let message = `String "expected key hash" in
    let value = `String (Format.asprintf "%a" Ast_typed.PP.type_expression t) in
    let content = `Assoc [
      ("message", message);
      ("location", Location.to_yojson loc);
      ("value", value);
    ] in
    json_error ~stage ~content
  | `Typer_expected_mutez (loc,t) ->
    let message = `String "expected mutez" in
    let content = `Assoc [
      ("message", message);
      ("location", Location.to_yojson loc);
      ("value", Ast_typed.Yojson.type_expression t);
    ] in
    json_error ~stage ~content
  | `Typer_expected_op_list (loc,t) ->
    let message = `String "expected operation lists" in
    let content = `Assoc [
      ("message", message);
      ("location", Location.to_yojson loc);
      ("value", Ast_typed.Yojson.type_expression t);
    ] in
    json_error ~stage ~content
  | `Typer_wrong_param_number (loc,name,expected,actual) ->
    let message = `String "constant with a wrong number of parameter" in
    let value = `String name in
    let expected = `Int expected in
    let actual = `Int (List.length actual) in
    let content = `Assoc [
      ("message", message);
      ("location", Location.to_yojson loc);
      ("value", value);
      ("actual", actual);
      ("expected", expected);
    ] in
    json_error ~stage ~content
  | `Typer_expected_ligo_code (loc,t) ->
    let message = `String "expected ligo code" in
    let value = Ast_typed.Yojson.type_expression t in
    let content = `Assoc [
      ("message", message);
      ("location", Location.to_yojson loc);
      ("value", value);
    ] in
    json_error ~stage ~content
  | `Typer_expected_michelson_code (loc,t) ->
    let message = `String "expected michelson code" in
    let value = Ast_typed.Yojson.type_expression t in
    let content = `Assoc [
      ("message", message);
      ("location", Location.to_yojson loc);
      ("value", value);
    ] in
    json_error ~stage ~content
  | `Typer_expected_sapling_state (loc,t) ->
    let message = `String "expected sapling_state" in
    let value = Ast_typed.Yojson.type_expression t in
    let content = `Assoc [
      ("message", message);
      ("location", Location.to_yojson loc);
      ("value", value);
    ] in
    json_error ~stage ~content
  | `Typer_expected_sapling_transaction (loc,t) ->
    let message = `String "expected sapling_transaction" in
    let value = Ast_typed.Yojson.type_expression t in
    let content = `Assoc [
      ("message", message);
      ("location", Location.to_yojson loc);
      ("value", value);
    ] in
    json_error ~stage ~content
  | `Typer_expected_function (loc,e) ->
    let message = `String "expected a function" in
    let value = `String (Format.asprintf "%a" Ast_typed.PP.type_expression e) in
    let content = `Assoc [
      ("message", message);
      ("location", Location.to_yojson loc);
      ("value", value);
    ] in
    json_error ~stage ~content
  | `Typer_expected_pair (loc,e) ->
    let message = `String "expected a pair" in
    let content = `Assoc [
      ("message", message);
      ("location", Location.to_yojson loc);
      ("value", Ast_typed.Yojson.type_expression e);
    ] in
    json_error ~stage ~content
  | `Typer_expected_list (loc,e) ->
    let message = `String "expected a list" in
    let content = `Assoc [
      ("message", message);
      ("location", Location.to_yojson loc);
      ("value", Ast_typed.Yojson.type_expression e);
    ] in
    json_error ~stage ~content
  | `Typer_expected_set (loc,e) ->
    let message = `String "expected a set" in
    let content = `Assoc [
      ("message", message);
      ("location", Location.to_yojson loc);
      ("value", Ast_typed.Yojson.type_expression e);
    ] in
    json_error ~stage ~content
  | `Typer_expected_map (loc,e) ->
    let message = `String "expected a map" in
    let content = `Assoc [
      ("message", message);
      ("location", Location.to_yojson loc);
      ("value", Ast_typed.Yojson.type_expression e);
    ] in
    json_error ~stage ~content
  | `Typer_expected_big_map (loc,e) ->
    let message = `String "expected a big map" in
    let content = `Assoc [
      ("message", message);
      ("location", Location.to_yojson loc);
      ("value", Ast_typed.Yojson.type_expression e);
    ] in
    json_error ~stage ~content
  | `Typer_expected_option (loc,e) ->
    let message = `String "expected an option" in
    let content = `Assoc [
      ("message", message);
      ("location", Location.to_yojson loc);
      ("value", Ast_typed.Yojson.type_expression e);
    ] in
    json_error ~stage ~content
  | `Typer_expected_int (loc,e) ->
    let message = `String "expected an int" in
    let content = `Assoc [
      ("message", message);
      ("location", Location.to_yojson loc);
      ("value", Ast_typed.Yojson.type_expression e);
    ] in
    json_error ~stage ~content
  | `Typer_expected_bool (loc,e) ->
    let message = `String "expected a bool" in
    let content = `Assoc [
      ("message", message);
      ("location", Location.to_yojson loc);
      ("value", Ast_typed.Yojson.type_expression e);
    ] in
    json_error ~stage ~content
  | `Typer_not_matching (loc,t1,t2) ->
    let message = `String "types not matching" in
    let content = `Assoc [
      ("message", message);
      ("location", Location.to_yojson loc);
      ("value_1", Ast_typed.Yojson.type_expression t1);
      ("value_2", Ast_typed.Yojson.type_expression t2);
    ] in
    json_error ~stage ~content
  | `Typer_not_annotated _ ->
    let message = `String "not annotated" in
    let content = `Assoc [
      ("message", message);
    ] in
    json_error ~stage ~content
  | `Typer_contract_not_annotated _ ->
    let message = `String "not annotated contract" in
    let content = `Assoc [
      ("message", message);
    ] in
    json_error ~stage ~content
  | `Typer_bad_substraction loc ->
    let message = `String "bad substraction, bad parameters" in
    let content = `Assoc [
      ("message", message);
      ("location", Location.to_yojson loc);
    ] in
    json_error ~stage ~content
  | `Typer_wrong_size (loc,t) ->
    let message = `String "should be of type map, list, string, byte or set" in
    let content = `Assoc [
      ("message", message);
      ("location", Location.to_yojson loc);
      ("value", Ast_typed.Yojson.type_expression t);
    ] in
    json_error ~stage ~content
  | `Typer_wrong_neg (loc,t) ->
    let message = `String "should be of type nat or int" in
    let content = `Assoc [
      ("message", message);
      ("location", Location.to_yojson loc);
      ("value", Ast_typed.Yojson.type_expression t);
    ] in
    json_error ~stage ~content
  | `Typer_wrong_not (loc,t) ->
    let message = `String "should be of type bool, nat or int" in
    let content = `Assoc [
      ("message", message);
      ("location", Location.to_yojson loc);
      ("value", Ast_typed.Yojson.type_expression t);
    ] in
    json_error ~stage ~content
  | `Typer_typeclass_error (loc,exps,acts) ->
    let open Simple_utils.PP_helpers in
    let printl printer ppf args =
      Format.fprintf ppf "%a" (list_sep printer (const " , ")) args in
    let message = `String "typeclass error" in
    let expected = `String (Format.asprintf "%a" (list_sep (printl Ast_typed.PP.type_expression) (const " or ")) exps) in
    let actual = `String (Format.asprintf "%a" (list_sep Ast_typed.PP.type_expression (const " or ")) acts) in
    let content = `Assoc [
      ("message", message);
      ("location", Location.to_yojson loc);
      ("expected", expected);
      ("actual", actual);
    ] in
    json_error ~stage ~content
  | `Typer_converter t ->
    let message = `String "converters can only be used on records or variants" in
    let value = `String (Format.asprintf "%a" Ast_typed.PP.type_expression t) in
    let content = `Assoc [
      ("message", message);
      ("value", value);
    ] in
    json_error ~stage ~content
  | `Typer_uncomparable_types (loc,a,b) ->
    let message = `String "those two types are not comparable" in
    let t1 = `String (Format.asprintf "%a" Ast_typed.PP.type_expression a) in
    let t2 = `String (Format.asprintf "%a" Ast_typed.PP.type_expression b) in
    let content = `Assoc [
      ("message", message);
      ("location", Location.to_yojson loc);
      ("type_1", t1);
      ("type_2", t2);
    ] in
    json_error ~stage ~content
  | `Typer_comparator_composed (loc,_a) ->
    let message = `String "Only composed types of not more than two element are allowed to be compared" in
    let content = `Assoc [
      ("message", message);
      ("location", Location.to_yojson loc);
    ] in
    json_error ~stage ~content
  | `Typer_constant_decl_tracer (name,ae,expected,err) ->
    let message = `String "typing constant declaration" in
    let loc = `String (Format.asprintf "%a" Location.pp ae.location) in
    let name = `String (Format.asprintf "%a" Ast_core.PP.expression_variable name) in
    let expected = `String (match expected with
      | None -> "(no annotation for the expected type)"
      | Some expected -> Format.asprintf "%a" Ast_typed.PP.type_expression expected) in
    let content = `Assoc [
      ("message", message) ;
      ("name", name) ;
      ("location", loc) ;
      ("expected", expected) ;
      ("children", error_jsonformat err) ;
    ] in
    json_error ~stage ~content
  | `Typer_unrecognized_type_constant e ->
    let message = `String "unrecognized type constant" in
    let value = `String (Format.asprintf "%a" Ast_core.PP.type_expression e) in
    let content = `Assoc [
      ("message", message) ;
      ("value", value) ;
    ] in
    json_error ~stage ~content
  | `Typer_expected_ascription t ->
    let message = `String "expected ascription" in
    let location = `String (Format.asprintf "%a" Location.pp t.location) in
    let value = `String (Format.asprintf "%a" Ast_core.PP.expression t) in
    let content = `Assoc [
      ("message", message) ;
      ("location", location) ;
      ("value", value) ;
    ] in
    json_error ~stage ~content
  | `Typer_different_types (a,b) ->
    let message = `String ("Types are different.\
      Expected these two types to be the same, but they're different") in
    let a = `String (Format.asprintf "%a" Ast_typed.PP.type_expression a) in
    let b = `String (Format.asprintf "%a" Ast_typed.PP.type_expression b) in
    let content = `Assoc [
      ("message", message) ;
      ("a", a) ;
      ("b", b) ;
    ] in
    json_error ~stage ~content
  | `Typer_variant_redefined_error loc ->
    let message = `String "Redefined variant" in
    let content = `Assoc [
      ("message", message) ;
      ("location", Location.to_yojson loc) ;
    ] in
    json_error ~stage ~content
  | `Typer_record_redefined_error loc ->
    let message = `String "Redefined record" in
    let content = `Assoc [
      ("message", message) ;
      ("location", Location.to_yojson loc)
    ] in
    json_error ~stage ~content
  | `Typer_constant_tag_number_of_arguments (loc, opa, opb, lena, lenb) ->
    let message = `String "different number of arguments to type constructors.\
      Expected these two n-ary type constructors to be the same, but they have different number\
      of arguments" in
    let a = `String (Format.asprintf "%a" Ast_typed.PP.constant_tag opa) in
    let b = `String (Format.asprintf "%a" Ast_typed.PP.constant_tag opb) in
    let op = `String (Format.asprintf "%a" Ast_typed.PP.constant_tag opa) in
    let len_a = `Int lena in
    let len_b = `Int lenb in
    let loc = `String loc in
    let content = `Assoc [
      ("message", message) ;
      ("a", a) ;
      ("b", b) ;
      ("op", op) ;
      ("len_a", len_a) ;
      ("len_b", len_b) ;
      ("thrown by compiler at", loc) ;
    ] in
    json_error ~stage ~content
  | `Typer_typeclass_not_a_rectangular_matrix ->
    let message = `String "internal error: typeclass is not represented as a rectangular matrix with one column per argument" in
    let content = `Assoc [
      ("message", message);
    ] in
    json_error ~stage ~content
  | `Typer_internal_error (loc, msg) ->
    let message = `String (Format.sprintf "internal error at %s: %s" loc msg) in
    let content = `Assoc [
      ("message", message);
    ] in
    json_error ~stage ~content
  | `Typer_pattern_do_not_conform_type (p,t) ->
    let message = `String "pattern do not conform type" in
    let pattern = (Stage_common.To_yojson.pattern Ast_core.Yojson.type_expression) p in
    let t = Ast_typed.Yojson.type_expression t in
    let content = `Assoc [
      ("message", message);
      ("type", t);
      ("pattern", pattern);
      ("location", Location.to_yojson p.location);
    ] in
    json_error ~stage ~content
  | `Typer_poly_not_applied (loc) ->
    let message = `String "polymorphic value is not applied enough" in
    let content = `Assoc [
      ("message", message);
      ("location", Location.to_yojson loc);
    ] in
    json_error ~stage ~content
  | `Typer_wrong_generalizable (loc, t) ->
    let message = `String (Format.asprintf "invalid type name: generalizable variable %a" Ast_core.PP.type_variable t) in
    let content = `Assoc [
      ("message", message);
      ("location", Location.to_yojson loc);
    ] in
    json_error ~stage ~content
