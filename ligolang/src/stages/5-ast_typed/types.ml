[@@@warning "-30-32"]

include Stage_common.Types

(* pseudo-typeclasses: interfaces that must be provided for arguments
   of the givent polymmorphic types. For now, only one typeclass can
   be specified for a given polymorphic type. The implementation is
   provided by the Comparable module *)
(*@ typeclass poly_unionfind comparable *)
(*@ typeclass poly_set       comparable *)
type ast_core_type_expression = Ast_core.type_expression

type te_lmap = row_element label_map
and type_meta = ast_core_type_expression option

and type_content =
  | T_variable of type_variable
  | T_constant of type_injection
  | T_sum      of rows
  | T_record   of rows
  | T_arrow    of ty_expr arrow
  | T_module_accessor of ty_expr module_access
  | T_singleton of literal
  | T_abstraction of ty_expr abstraction
  | T_for_all of ty_expr abstraction

and type_injection = {
  language : string ;
  injection : Ligo_string.t ;
  parameters : ty_expr list ;
}

and rows = {
  content : row_element label_map;
  layout : layout ;
}

and te_list = type_expression list

and annot_option = string option

and row_element = type_expression row_element_mini_c

and type_map_args = {
    k : type_expression;
    v : type_expression;
  }

and michelson_or_args = {
    l : type_expression;
    r : type_expression;
  }

and type_expression = {
    type_content: type_content;
    type_meta: type_meta;
    orig_var: type_variable option ;
    location: location;
  }
and ty_expr = type_expression

and expression_variable_list = expression_variable list
and type_expression_list = type_expression list

and matching_content_case = {
    constructor : label ;
    pattern : expression_variable ;
    body : expression ;
  }

and matching_content_case_list = matching_content_case list

and matching_content_variant = {
    cases: matching_content_case_list;
    tv: type_expression;
  }

and matching_content_record = {
  fields : (expression_variable * type_expression) label_map;
  body : expression;
  tv : type_expression;
}

and matching_expr =
  | Match_variant of matching_content_variant
  | Match_record of matching_content_record

and declaration_loc = declaration location_wrap

and module' = declaration_loc list

and module_with_unification_vars = Module_With_Unification_Vars of module'

and module_fully_typed = Module_Fully_Typed of module'

and type_attribute = { public : bool }

and module_attribute = { public : bool }

(* A Declaration_constant is described by
 *   a name + a type-annotated expression
 *   a boolean indicating whether it should be inlined
 *   the environment before the declaration (the original environment)
 *   the environment after the declaration (i.e. with that new declaration added to the original environment). *)
and declaration_constant = {
    name : string option ;
    binder : expression_variable ;
    expr : expression ;
    attr : known_attributes ;
  }

and declaration_type = {
    type_binder : type_variable ;
    type_expr   : type_expression ;
    type_attr   : type_attribute
  }

and declaration_module = {
    module_binder : module_variable ;
    module_       : module_fully_typed ;
    module_attr   : module_attribute
  }

and declaration' =
  | Declaration_constant of declaration_constant
  | Declaration_type of declaration_type
  | Declaration_module of declaration_module
  | Module_alias       of module_alias

and declaration = declaration'

and expression = {
    expression_content: expression_content ;
    location: location ;
    type_expression: type_expression ;
  }

and expr = expression

and map_kv = {
    key : expression ;
    value : expression ;
  }

and look_up = {
    ds : expression;
    ind : expression;
  }

and expression_label_map = expression label_map
and expression_list = expression list

and expression_content =
  (* Base *)
  | E_literal of literal
  | E_constant of constant (* For language constants, like (Cons hd tl) or (plus i j) *)
  | E_variable of expression_variable
  | E_application of application
  | E_lambda of lambda
  | E_recursive of recursive
  | E_let_in of let_in
  | E_type_in of (expr, ty_expr) type_in
  | E_mod_in of mod_in
  | E_mod_alias of expr mod_alias
  | E_raw_code of raw_code
  | E_type_inst of type_inst
  (* Variant *)
  | E_constructor of constructor (* For user defined constructors *)
  | E_matching of matching
  (* Record *)
  | E_record of expression_label_map
  | E_record_accessor of record_accessor
  | E_record_update   of record_update
  | E_module_accessor of expression module_access

and type_inst = {
    forall: expression ;
    type_: type_expression ;
  }

and constant = {
    cons_name: constant' ;
    arguments: expression_list ;
  }

and application = {
  lamb: expression ;
  args: expression ;
  }

and lambda =  {
    binder: expression_variable ;
    result: expression ;
  }

and let_in = {
    let_binder: expression_variable ;
    rhs: expression ;
    let_result: expression ;
    attr: known_attributes ;
  }

and mod_in = {
    module_binder: module_variable ;
    rhs: module_fully_typed ;
    let_result: expression ;
  }

and raw_code = {
  language : string;
  code : expression;
  }

and recursive = {
  fun_name : expression_variable;
  fun_type : type_expression;
  lambda : lambda;
  }

and constructor = {
    constructor: label;
    element: expression ;
  }

and record_accessor = {
    record: expression ;
    path: label ;
  }

and record_update = {
    record: expression ;
    path: label ;
    update: expression ;
  }

and matching = {
    matchee: expression ;
    cases: matching_expr ;
  }

and ascription = {
    anno_expr: expression ;
    type_annotation: type_expression ;
  }

and environment_element_definition =
  | ED_binder
  | ED_declaration of environment_element_definition_declaration

and environment_element_definition_declaration = {
    expression: expression ;
    free_variables: free_variables ;
    attr : known_attributes ;
  }

and free_variables = expression_variable list

and environment_element = {
    type_value: type_expression ;
    definition: environment_element_definition ;
  }

and expression_environment = environment_binding list

and environment_binding = {
    expr_var: expression_variable ;
    env_elt: environment_element ;
    public: bool;
  }

and type_environment = type_environment_binding list

and type_or_kind = Ty of type_expression | Kind of unit

and type_environment_binding = {
    type_variable: type_variable ;
    type_: type_or_kind ;
    public: bool;
  }

and module_environment = module_environment_binding list

and module_environment_binding = {
  module_variable : module_variable ;
  module_ : environment ;
  public: bool;
}
and environment = {
  expression_environment: expression_environment ;
  type_environment: type_environment ;
  module_environment : module_environment ;
  }

(* Solver types


   The solver types are not actually part of the AST,
   so they could be moved to a separate file, but doing so would
   require updating every use of the Ast_typed.Types module to also
   include the solver types (a lot of work). Also, there is a lot of
   semi-duplication between the AST and the solver, so it's best to
   keep the two together until that gets refactored. *)

(* core *)
module ConstraintIdentifier = struct
  type t = T of int64
  let counter : int64 ref = ref Int64.zero 
  let fresh () = 
    let res = !counter in
    counter := Int64.succ !counter;
    T res

end

(* add information on the type or the kind for operator *)
type constant_tag =
  | C_arrow        (* * -> * -> * *)
  | C_option       (* * -> * *)
  | C_map          (* * -> * -> * *)
  | C_big_map      (* * -> * -> * *)
  | C_list         (* * -> * *)
  | C_set          (* * -> * *)
  | C_unit         (* * *)
  | C_string       (* * *)
  | C_nat          (* * *)
  | C_mutez        (* * *)
  | C_timestamp    (* * *)
  | C_int          (* * *)
  | C_address      (* * *)
  | C_bytes        (* * *)
  | C_key_hash     (* * *)
  | C_key          (* * *)
  | C_signature    (* * *)
  | C_operation    (* * *)
  | C_contract     (* * -> * *)
  | C_chain_id     (* * *)
  | C_bls12_381_g1 (* * *)
  | C_bls12_381_g2 (* * *)
  | C_bls12_381_fr (* * *)

type row_tag =
  | C_record    (* ( label , * ) … -> * *)
  | C_variant   (* ( label , * ) … -> * *)
