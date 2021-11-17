module TYPE_VARIABLE_ABSTRACTION = functor (Type_variable : sig type t end) -> struct
  module type S = sig
    (* this is dangerous, it ignores repr etc. because it works on type_value *)

    module Types : sig
      module ConstraintIdentifier : sig
        type t = T of int64
        val fresh : unit -> t
      end
      type type_variable = Type_variable.t
      type constraint_identifier = ConstraintIdentifier.t
      type constant_tag
      type row_tag =
        | C_record    (* ( label , * ) … -> * *)
        | C_variant   (* ( label , * ) … -> * *)
      type label
      module LMap : Map.S with type key = label
      type 'a label_map = 'a LMap.t
      type typeclass = type_value list list
      and type_value_ =
        | P_variable     of type_variable
        | P_row          of p_row
        | P_constant     of p_constant
        | P_forall       of p_forall
        | P_abs          of p_abs
        | P_constraint   of p_constraint
        | P_apply        of p_apply
      
      and p_abs = { arg: type_variable; ret: type_value }
      and p_constraint = { pc: type_constraint }
      and p_constraints = type_constraint list
      and p_forall = {
        binder      : type_variable ;
        constraints : p_constraints ;
        body        : type_value ;
      }
      and p_constant = {
        p_ctor_tag : constant_tag ;
        p_ctor_args : p_ctor_args ;
      }
      and p_apply = {
        tf : type_value ;
        targ : type_value ;
      }
      and p_row = {
        p_row_tag  : row_tag ;
        p_row_args : row_lmap ;
      }
      and type_value = type_value_ Location.wrap
      and p_ctor_args = type_value list

      and row_value = {
        associated_value : type_value;
        michelson_annotation : string option;
        decl_pos : int;
      }

      and row_lmap = row_value label_map

      and c_alias = {
        reason_alias_simpl : string ;
        a : type_variable ;
        b : type_variable ;
      }

      and c_constructor_simpl = {
        reason_constr_simpl : string ;
        id_constructor_simpl : constraint_identifier ;
        original_id  : constraint_identifier option ;
        tv : type_variable;
        c_tag : constant_tag;
        tv_list : type_variable list;
      }

      and c_row_simpl = {
        reason_row_simpl : string ;
        id_row_simpl : constraint_identifier ;
        original_id  : constraint_identifier option ;
        tv : type_variable;
        r_tag : row_tag;
        tv_map : row_variable label_map;
      }

      and row_variable = {
        associated_variable : type_variable;
        michelson_annotation : string option;
        decl_pos : int;
      }

      and c_typeclass_simpl = {
        reason_typeclass_simpl : string ;
        id_typeclass_simpl : constraint_identifier ;
        original_id        : constraint_identifier option ;
        tc_bound           : type_variable list; (* NOTE: these variables are disjoint from unification variables; using repr on them is harmless but useless *)
        tc_constraints     : type_constraint_simpl list;
        tc   : typeclass          ;
        args : type_variable list ;
      }
      and c_access_label_simpl = {
        reason_access_label_simpl : string ;
        id_access_label_simpl : constraint_identifier ;
        record_type : type_variable ;
        label : label ;
        tv : type_variable ;
      }
      and c_poly_simpl = {
        reason_poly_simpl : string ;
        id_poly_simpl : constraint_identifier ;
        original_id   : constraint_identifier option ;
        tv     : type_variable ;
        forall : p_forall ;
      }
      and c_apply_simpl = {
        id_apply_simpl : constraint_identifier ;
        reason_apply_simpl : string ;
        f: type_variable;
        arg: type_variable;
      }
      and c_abs_simpl = {
        id_abs_simpl : constraint_identifier ;
        reason_abs_simpl : string ;
        tv: type_variable;
        param: type_variable;
        body: type_value;
      }
      and type_constraint_simpl =
        | SC_Apply        of c_apply_simpl
        | SC_Abs          of c_abs_simpl
        | SC_Constructor  of c_constructor_simpl
        | SC_Alias        of c_alias
        | SC_Poly         of c_poly_simpl
        | SC_Typeclass    of c_typeclass_simpl
        | SC_Access_label of c_access_label_simpl
        | SC_Row          of c_row_simpl

      and constructor_or_row = [
        | `Constructor of c_constructor_simpl
        | `Row of c_row_simpl
      ]

      and type_constraint_ =
        | C_equation of c_equation
        | C_typeclass of c_typeclass
        | C_access_label of c_access_label
        | C_apply of c_apply

      and c_equation = {
        aval : type_value ;
        bval : type_value ;
      }

      and tc_args = type_value list

      and c_typeclass = {
        tc_bound : type_variable list; (* NOTE: these variables are disjoint from unification variables; using repr on them is harmless but useless *)
        tc_constraints : type_constraint list;
        tc_args : tc_args ;
        original_id : constraint_identifier option ;
        typeclass : typeclass ;
      }

      and c_access_label = {
        c_access_label_record_type : type_value ;
        accessor : label ;
        c_access_label_tvar : type_variable ;
      }

      and c_apply = {
        f: type_variable;
        arg: type_variable;
      }

      and type_constraint = {
        reason : string ;
        c : type_constraint_ ;
      }

      type axiom = HandWaved of string

      type proof_trace =
        | Axiom of axiom

      type update = {
        remove_constraints : type_constraint_simpl list ;
        add_constraints : type_constraint list ;
        add_constraints_simpl : type_constraint_simpl list ;
        proof_trace : proof_trace ;
      }
      type updates = update list

      type expression_
      and expression_variable = expression_ Var.t Location.wrap

      type module_variable = string

      type type_expression
      type expression
    end
    val cast_access_to_simplifier_do_not_do_this_do_not_use_this : (Ast_core.Types.type_constraint -> Ast_core.Types.type_constraint_simpl list) -> (Types.type_constraint -> Types.type_constraint_simpl list)
    module Substitution : sig
      module Pattern : sig
        open Types
        val type_value : tv:type_value -> substs:Type_variable.t * type_value -> type_value
      end
    end

    module Compare : sig
      open Types
      type 'a comparator = 'a -> 'a -> int
      val (<?) : int -> (unit -> int) -> int
      val cmp2 : 'a comparator -> 'a -> 'a -> 'b comparator -> 'b -> 'b -> int
      val c_constructor_simpl : c_constructor_simpl comparator
      val c_row_simpl : c_row_simpl comparator
      val c_poly_simpl : c_poly_simpl comparator
      val c_access_label_simpl : c_access_label_simpl comparator
      val c_typeclass_simpl : c_typeclass_simpl comparator
      val c_apply_simpl : c_apply_simpl comparator
      val c_abs_simpl : c_abs_simpl comparator
      val constructor_or_row : constructor_or_row comparator
      val label : label comparator
      val constant_tag : constant_tag comparator
      val row_tag : row_tag comparator
      val constraint_identifier : constraint_identifier comparator
      val type_variable : type_variable comparator
      val type_value : type_value comparator
      val typeclass  : typeclass comparator
    end

    module PP : sig
      open Types
      type 'a pretty_printer = Format.formatter -> 'a -> unit
      val c_constructor_simpl : c_constructor_simpl pretty_printer
      val c_constructor_simpl_short : c_constructor_simpl pretty_printer
      val c_row_simpl : c_row_simpl pretty_printer
      val c_row_simpl_short : c_row_simpl pretty_printer
      val c_poly_simpl_short : c_poly_simpl pretty_printer
      val c_typeclass_simpl : c_typeclass_simpl pretty_printer
      val c_typeclass_simpl_short : c_typeclass_simpl pretty_printer
      val c_access_label_simpl : c_access_label_simpl pretty_printer
      val c_access_label_simpl_short : c_access_label_simpl pretty_printer
      val constructor_or_row_short : constructor_or_row pretty_printer
      val c_apply_simpl_short : c_apply_simpl pretty_printer
      val c_abs_simpl_short : c_abs_simpl pretty_printer
      val constraint_identifier : constraint_identifier pretty_printer
      val type_variable : type_variable pretty_printer
      val type_value_short : type_value pretty_printer
      val type_constraint : type_constraint pretty_printer
      val type_constraint_simpl_short : type_constraint_simpl pretty_printer
    end

    module Yojson : sig
      open Types
      type 'a json_printer = 'a -> Yojson.Safe.t
      val constructor_or_row : constructor_or_row json_printer
      val c_constructor_simpl : c_constructor_simpl json_printer
      val c_row_simpl : c_row_simpl json_printer
      val c_poly_simpl : c_poly_simpl json_printer
      val c_typeclass_simpl : c_typeclass_simpl json_printer
      val c_abs_simpl : c_abs_simpl json_printer
      val c_apply_simpl : c_apply_simpl json_printer
      val c_access_label_simpl : c_access_label_simpl json_printer
    end

    module Solver_types : sig
      open Types
      type ('selector_output , 'errors) propagator = raise:'errors Trace.raise -> 'selector_output -> (type_variable -> type_variable) -> updates
    end

    module Misc : sig
      open Types
      val c_equation : type_value -> type_value -> string -> type_constraint
    end

    module Reasons : sig
      type t =
        | Forall
        | Forall_TC
        | Builtin_type
        | Propagator_break_ctor of string
        | Propagator_access_label of string
        | Propagator_specialize_apply
        | Propagator_specialize_tf
        | Propagator_specialize_targ
        | Propagator_specialize_eq
        | Todo of string

      val pp : Format.formatter -> t -> unit
      val wrap : t -> 'v -> 'v Location.wrap
    end

    module Axioms : sig
      open Types
      val f_equal : axiom
      val specialize : axiom
    end

    module Core : sig
      open Types
      val fresh_type_variable : ?name:string -> unit -> type_variable
    end

    module Typelang : sig
      open Types
      val type_level_eval : type_value -> type_value * type_constraint list
      val check_applied : ((type_value * _) as 'a) -> 'a
    end

    module Errors : sig
      type typer_error = [
        | `Typer_missing_funarg_annotation of Types.expression_variable
        | `Typer_michelson_comb_no_record of Location.t
        | `Typer_michelson_comb_no_variant of Location.t
        | `Typer_unbound_module_variable of Ast_core.Environment.t * Types.module_variable * Location.t
        | `Typer_unbound_type_variable of Ast_core.Environment.t * Types.type_variable * Location.t
        | `Typer_unbound_variable of Ast_core.Environment.t * Types.expression_variable * Location.t
        | `Typer_match_missing_case of Types.label list * Types.label list * Location.t
        | `Typer_match_extra_case of Types.label list * Types.label list * Location.t
        | `Typer_unbound_constructor of Ast_core.Environment.t * Types.label * Location.t
        | `Typer_redundant_constructor of Ast_core.Environment.t * Types.label * Location.t
        | `Typer_type_constant_wrong_number_of_arguments of Types.type_variable* int * int * Location.t
        | `Typer_michelson_or_no_annotation of Types.label * Location.t
        | `Typer_module_tracer of Ast_core.module_ * typer_error
        | `Typer_constant_declaration_tracer of Ast_core.expression_variable * Ast_core.expression * (Types.type_expression option) * typer_error
        | `Typer_match_error of Ast_core.matching_expr * Ast_core.type_expression * Location.t
        | `Typer_needs_annotation of Ast_core.expression * string
        | `Typer_fvs_in_create_contract_lambda of Ast_core.expression * Types.expression_variable
        | `Typer_create_contract_lambda of Ast_core.constant' * Ast_core.expression
        | `Typer_should_be_a_function_type of Types.type_expression * Ast_core.expression
        | `Typer_bad_record_access of Ast_core.label * Ast_core.expression * Types.type_expression * Location.t
        | `Typer_expression_tracer of Ast_core.expression * typer_error
        | `Typer_record_access_tracer of Types.expression * typer_error
        | `Typer_assert_equal of Location.t * Types.type_expression * Types.type_expression
        | `Typer_corner_case of string
        | `Typer_bad_collect_loop of Types.type_expression * Location.t
        | `Typer_declaration_order_record of Location.t
        | `Typer_too_small_record of Location.t
        | `Typer_expected_record of Location.t * Types.type_expression
        | `Typer_expected_variant of Location.t * Types.type_expression
        | `Typer_wrong_param_number of Location.t * string * int * Types.type_expression list
        | `Typer_expected_function of Location.t * Types.type_expression
        | `Typer_expected_pair of Location.t * Types.type_expression
        | `Typer_expected_list of Location.t * Types.type_expression
        | `Typer_expected_set of Location.t * Types.type_expression
        | `Typer_expected_map of Location.t * Types.type_expression
        | `Typer_expected_big_map of Location.t * Types.type_expression
        | `Typer_expected_option of Location.t * Types.type_expression
        | `Typer_expected_nat of Location.t * Types.type_expression
        | `Typer_expected_bytes of Location.t * Types.type_expression
        | `Typer_expected_key of Location.t * Types.type_expression
        | `Typer_expected_signature of Location.t * Types.type_expression
        | `Typer_expected_contract of Location.t * Types.type_expression
        | `Typer_expected_string of Location.t * Types.type_expression
        | `Typer_expected_key_hash of Location.t * Types.type_expression
        | `Typer_expected_mutez of Location.t * Types.type_expression
        | `Typer_expected_op_list of Location.t * Types.type_expression
        | `Typer_expected_int of Location.t * Types.type_expression
        | `Typer_expected_bool of Location.t * Types.type_expression
        | `Typer_expected_ticket of Location.t * Types.type_expression
        | `Typer_expected_sapling_transaction of Location.t * Types.type_expression
        | `Typer_expected_sapling_state of Location.t * Types.type_expression
        | `Typer_not_matching of Location.t * Types.type_expression * Types.type_expression
        | `Typer_not_annotated of Location.t
        | `Typer_bad_substraction of Location.t
        | `Typer_wrong_size of Location.t * Types.type_expression
        | `Typer_wrong_neg of Location.t * Types.type_expression
        | `Typer_wrong_not of Location.t * Types.type_expression
        | `Typer_typeclass_error of Location.t * Types.type_expression list list * Types.type_expression list
        | `Typer_converter of Types.type_expression
        | `Typer_uncomparable_types of Location.t * Types.type_expression * Types.type_expression
        | `Typer_comparator_composed of Location.t * Types.type_expression
        | `Typer_constant_decl_tracer of Ast_core.expression_variable * Ast_core.expression * Types.type_expression option * typer_error
        | `Typer_unrecognized_type_constant of Ast_core.type_expression
        | `Typer_expected_ascription of Ast_core.expression
        | `Typer_different_types of Types.type_expression * Types.type_expression
        | `Typer_variant_redefined_error of Location.t
        | `Typer_record_redefined_error of Location.t
        | `Typer_constant_tag_number_of_arguments of string * Types.constant_tag * Types.constant_tag * int * int
        | `Typer_typeclass_not_a_rectangular_matrix
        | `Typer_could_not_remove of Types.type_constraint_simpl
        | `Typer_internal_error of string * string
        | `Trace_debug of string * typer_error
        | `Typer_pattern_do_not_match of Location.t
        | `Typer_label_do_not_match of Types.label * Types.label * Location.t
        | `Typer_solver_no_progress of string
        | `Typer_different_typeclasses of Ast_core.c_typeclass_simpl * Ast_core.c_typeclass_simpl
      ]
    end
  end
end
