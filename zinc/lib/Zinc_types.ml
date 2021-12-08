open Zinc_utils

type address = string [@@deriving show {with_path = false}, eq, yojson]

type contract = string * address option
[@@deriving show {with_path = false}, eq, yojson]

module type Zinc_description = sig
  module Zinc :
    sig
      type core_instruction =
          Grab
        | Return
        | PushRetAddr of t
        | Apply
        | Access of label
        | Closure of t
        | EndLet
      and plain_old_data =
          Bool of bool
        | String of address
        | Num of Z.t
        | Mutez of Z.t
        | Nil
        | Bytes of bytes
        | Address of address
        | Key of address
        | Hash of address
      and adt =
          MakeRecord of label
        | RecordAccess of label
        | MakeVariant of address
        | MatchVariant of (address * t) list
      and operation = Eq | Add | Cons | HashKey
      and domain_specific_operation =
          ChainID
        | Contract_opt
        | MakeTransaction
      and control_flow = Failwith
      and instruction =
          Core of core_instruction
        | Plain_old_data of plain_old_data
        | Adt of adt
        | Operation of operation
        | Domain_specific_operation of domain_specific_operation
        | Control_flow of control_flow
      and t = instruction list
      val pp_core_instruction : Format.formatter -> core_instruction -> unit
      val show_core_instruction : core_instruction -> address
      val pp_plain_old_data : Format.formatter -> plain_old_data -> unit
      val show_plain_old_data : plain_old_data -> address
      val pp_adt : Format.formatter -> adt -> unit
      val show_adt : adt -> address
      val pp_operation : Format.formatter -> operation -> unit
      val show_operation : operation -> address
      val pp_domain_specific_operation :
        Format.formatter -> domain_specific_operation -> unit
      val show_domain_specific_operation :
        domain_specific_operation -> address
      val pp_control_flow : Format.formatter -> control_flow -> unit
      val show_control_flow : control_flow -> address
      val pp_instruction : Format.formatter -> instruction -> unit
      val show_instruction : instruction -> address
      val pp : Format.formatter -> t -> unit
      val show : t -> address
      val equal_core_instruction :
        core_instruction -> core_instruction -> bool
      val equal_plain_old_data : plain_old_data -> plain_old_data -> bool
      val equal_adt : adt -> adt -> bool
      val equal_operation : operation -> operation -> bool
      val equal_domain_specific_operation :
        domain_specific_operation -> domain_specific_operation -> bool
      val equal_control_flow : control_flow -> control_flow -> bool
      val equal_instruction : instruction -> instruction -> bool
      val equal : t -> t -> bool
      val core_instruction_to_yojson : core_instruction -> Yojson.Safe.t
      val core_instruction_of_yojson :
        Yojson.Safe.t ->
        core_instruction Ppx_deriving_yojson_runtime.error_or
      val plain_old_data_to_yojson : plain_old_data -> Yojson.Safe.t
      val plain_old_data_of_yojson :
        Yojson.Safe.t -> plain_old_data Ppx_deriving_yojson_runtime.error_or
      val adt_to_yojson : adt -> Yojson.Safe.t
      val adt_of_yojson :
        Yojson.Safe.t -> adt Ppx_deriving_yojson_runtime.error_or
      val operation_to_yojson : operation -> Yojson.Safe.t
      val operation_of_yojson :
        Yojson.Safe.t -> operation Ppx_deriving_yojson_runtime.error_or
      val domain_specific_operation_to_yojson :
        domain_specific_operation -> Yojson.Safe.t
      val domain_specific_operation_of_yojson :
        Yojson.Safe.t ->
        domain_specific_operation Ppx_deriving_yojson_runtime.error_or
      val control_flow_to_yojson : control_flow -> Yojson.Safe.t
      val control_flow_of_yojson :
        Yojson.Safe.t -> control_flow Ppx_deriving_yojson_runtime.error_or
      val instruction_to_yojson : instruction -> Yojson.Safe.t
      val instruction_of_yojson :
        Yojson.Safe.t -> instruction Ppx_deriving_yojson_runtime.error_or
      val to_yojson : t -> Yojson.Safe.t
      val of_yojson : Yojson.Safe.t -> t Ppx_deriving_yojson_runtime.error_or
      type nonliteral_value =
          Contract of contract
        | Chain_operation of chain_operation
      and chain_operation = Transaction of Z.t * contract
      val pp_nonliteral_value : Format.formatter -> nonliteral_value -> unit
      val show_nonliteral_value : nonliteral_value -> address
      val pp_chain_operation : Format.formatter -> chain_operation -> unit
      val show_chain_operation : chain_operation -> address
      val equal_nonliteral_value :
        nonliteral_value -> nonliteral_value -> bool
      val equal_chain_operation : chain_operation -> chain_operation -> bool
      val nonliteral_value_to_yojson : nonliteral_value -> Yojson.Safe.t
      val nonliteral_value_of_yojson :
        Yojson.Safe.t ->
        nonliteral_value Ppx_deriving_yojson_runtime.error_or
      val chain_operation_to_yojson : chain_operation -> Yojson.Safe.t
      val chain_operation_of_yojson :
        Yojson.Safe.t -> chain_operation Ppx_deriving_yojson_runtime.error_or
    end
  type program = (address * Zinc.t) list
  val pp_program : Format.formatter -> program -> unit
  val show_program : program -> address
  val equal_program : program -> program -> bool
  val program_to_yojson : program -> Yojson.Safe.t
  val program_of_yojson :
    Yojson.Safe.t -> program Ppx_deriving_yojson_runtime.error_or
  module rec Env_item :
    sig
      type t =
          Z of Zinc.instruction
        | NonliteralValue of Zinc.nonliteral_value
        | Clos of Clos.t
        | Record of Stack_item.t array
        | List of Stack_item.t list
        | Variant of address * Stack_item.t
      val pp : Format.formatter -> t -> unit
      val show : t -> address
      val equal : t -> t -> bool
      val to_yojson : t -> Yojson.Safe.t
      val of_yojson : Yojson.Safe.t -> t Ppx_deriving_yojson_runtime.error_or
    end
  and Stack_item :
    sig
      type t =
          Z of Zinc.instruction
        | NonliteralValue of Zinc.nonliteral_value
        | Clos of Clos.t
        | Record of t array
        | List of t list
        | Variant of address * t
        | Marker of Zinc.t * Env_item.t list
      val pp : Format.formatter -> t -> unit
      val show : t -> address
      val equal : t -> t -> bool
      val to_yojson : t -> Yojson.Safe.t
      val of_yojson : Yojson.Safe.t -> t Ppx_deriving_yojson_runtime.error_or
    end
  and Clos :
    sig
      type t = { code : Zinc.t; env : Env_item.t list; }
      val pp : Format.formatter -> t -> unit
      val show : t -> address
      val equal : t -> t -> bool
      val to_yojson : t -> Yojson.Safe.t
      val of_yojson : Yojson.Safe.t -> t Ppx_deriving_yojson_runtime.error_or
    end
  type env = Env_item.t list
  val pp_env : Format.formatter -> env -> unit
  val show_env : env -> address
  val equal_env : env -> env -> bool
  val env_to_yojson : env -> Yojson.Safe.t
  val env_of_yojson :
    Yojson.Safe.t -> env Ppx_deriving_yojson_runtime.error_or
  type stack = Stack_item.t list
  val pp_stack : Format.formatter -> stack -> unit
  val show_stack : stack -> address
  val equal_stack : stack -> stack -> bool
  val stack_to_yojson : stack -> Yojson.Safe.t
  val stack_of_yojson :
    Yojson.Safe.t -> stack Ppx_deriving_yojson_runtime.error_or
  type interpreter_input = Zinc.t * env * stack
  val pp_interpreter_input : Format.formatter -> interpreter_input -> unit
  val show_interpreter_input : interpreter_input -> address
  val equal_interpreter_input :
    interpreter_input -> interpreter_input -> bool
  val interpreter_input_to_yojson : interpreter_input -> Yojson.Safe.t
  val interpreter_input_of_yojson :
    Yojson.Safe.t -> interpreter_input Ppx_deriving_yojson_runtime.error_or
  module Interpreter_output :
    sig
      type t = Success of env * stack | Failure of address
      val pp : Format.formatter -> t -> unit
      val show : t -> address
      val equal : t -> t -> bool
      val to_yojson : t -> Yojson.Safe.t
      val of_yojson : Yojson.Safe.t -> t Ppx_deriving_yojson_runtime.error_or
    end
  type interpreter_context = {
    get_contract_opt : address -> contract option;
  }
  module Utils :
    sig
      val unit_record_stack : Stack_item.t
      val unit_record_env : Env_item.t
    end
end


module Zinc_description = struct
  module Zinc = struct
    type core_instruction =
      | Grab
      | Return
      | PushRetAddr of t
      | Apply
      | Access of int
      | Closure of t
      | EndLet
    [@@deriving show {with_path = false}, eq, yojson]

    and plain_old_data =
      | Bool of bool
      | String of string
      | Num of Z.t
      | Mutez of Z.t
      | Nil
      | Bytes of bytes
      | Address of address
      | Key of string
      | Hash of string
    [@@deriving show {with_path = false}, eq, yojson]

    and adt =
      | MakeRecord of int
      | RecordAccess of label
      | MakeVariant of variant_label
      | MatchVariant of (variant_label * t) list
    [@@deriving show {with_path = false}, eq, yojson]

    and operation = Eq | Add | Cons | HashKey
    [@@deriving show {with_path = false}, eq, yojson]

    and domain_specific_operation = ChainID | Contract_opt | MakeTransaction
    [@@deriving show {with_path = false}, eq, yojson]

    and control_flow = Failwith
    [@@deriving show {with_path = false}, eq, yojson]

    and instruction =
      (*
      Everything in here should be safe and trustworthy. Our assumption is that an adversary
      can create whatever zinc they want and provide it as code to the interpreter.
      The code is guaranteed
  *)
      | Core of core_instruction
      | Plain_old_data of plain_old_data
      | Adt of adt
      | Operation of operation
      | Domain_specific_operation of domain_specific_operation
      | Control_flow of control_flow
    [@@deriving show {with_path = false}, eq, yojson]

    and t = instruction list [@@deriving show {with_path = false}, eq, yojson]

    (*
    Not all zinc values can be expressed directly in code as literals.
    So they're represented as a seperate type.
  *)
    type nonliteral_value =
      | Contract of contract
      | Chain_operation of chain_operation
    [@@deriving show {with_path = false}, eq, yojson]

    and chain_operation =
      | Transaction of Z.t * contract (* todo: add parameter *)
    [@@deriving show {with_path = false}, eq, yojson]
  end

  type program = (string * Zinc.t) list
  [@@deriving show {with_path = false}, eq, yojson]

  module rec Env_item : sig
    type t =
      | Z of Zinc.instruction
      | NonliteralValue of Zinc.nonliteral_value
      | Clos of Clos.t
      | Record of Stack_item.t LMap.t
      | List of Stack_item.t list
      | Variant of variant_label * Stack_item.t
    [@@deriving show, eq, yojson]
  end = struct
    type t =
      | Z of Zinc.instruction
      | NonliteralValue of Zinc.nonliteral_value
      | Clos of Clos.t
      | Record of Stack_item.t LMap.t
      | List of Stack_item.t list
      | Variant of variant_label * Stack_item.t
    [@@deriving show, eq, yojson]
  end

  and Stack_item : sig
    type t =
      | Z of Zinc.instruction
      | NonliteralValue of Zinc.nonliteral_value
      | Clos of Clos.t
      | Record of t LMap.t
      | List of t list
      | Variant of variant_label * t
      | Marker of Zinc.t * Env_item.t list
    [@@deriving show, eq, yojson]
  end = struct
    type t =
      | Z of Zinc.instruction
      | NonliteralValue of Zinc.nonliteral_value
      | Clos of Clos.t
      | Record of t LMap.t
      | List of t list
      | Variant of variant_label * t
      | Marker of Zinc.t * Env_item.t list
    [@@deriving show, eq, yojson]
  end

  and Clos : sig
    type t = {code : Zinc.t; env : Env_item.t list}
    [@@deriving show, eq, yojson]
  end = struct
    type t = {code : Zinc.t; env : Env_item.t list}
    [@@deriving show, eq, yojson]
  end

  type env = Env_item.t list [@@deriving show, eq, yojson]

  type stack = Stack_item.t list [@@deriving show, eq, yojson]

  type interpreter_input = Zinc.t * env * stack [@@deriving show, eq, yojson]

  module Interpreter_output = struct
    type t = Success of env * stack | Failure of string
    [@@deriving show, eq, yojson]
  end

  type interpreter_context = {get_contract_opt : address -> contract option}

  module Utils = struct
    let unit_record_stack = Stack_item.Record LMap.empty

    let unit_record_env = Env_item.Record LMap.empty
  end
end
