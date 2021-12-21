open Zinc_utils

module type With_yojson = sig
  type t

  val to_yojson : t -> Yojson.Safe.t

  val of_yojson : Yojson.Safe.t -> t Ppx_deriving_yojson_runtime.error_or
end

module type With_string = sig
  type t

  val to_string : t -> string

  val of_string : string -> t option
end

module type With_show = sig
  type t

  val to_string : t -> string

  val pp : Format.formatter -> t -> unit
end

module type With_eq = sig
  type t

  val equal : t -> t -> bool
end

module type With_default_derivation = sig
  type t

  include With_eq with type t := t

  include With_show with type t := t

  include With_yojson with type t := t
end

module type With_domain_derivation = sig
  type t

  include With_eq with type t := t

  include With_yojson with type t := t

  include With_string with type t := t
end

module type Domain_types = sig
  module Hash : sig
    type t

    include With_domain_derivation with type t := t
  end

  module Key : sig
    type t

    include With_domain_derivation with type t := t
  end

  module Key_hash : sig
    type t

    include With_domain_derivation with type t := t
  end

  module Address : sig
    type t

    include With_domain_derivation with type t := t
  end

  module Contract : sig
    type t

    include With_domain_derivation with type t := t
  end

  module Chain_id : sig
    type t

    include With_domain_derivation with type t := t
  end
end

module type S = sig
  module Zinc : sig
    module Key : sig
      type t

      include With_domain_derivation with type t := t

      include With_show with type t := t
    end

    module Key_hash : sig
      type t

      include With_domain_derivation with type t := t

      include With_show with type t := t
    end

    module Address : sig
      type t

      include With_domain_derivation with type t := t

      include With_show with type t := t
    end

    module Contract : sig
      type t

      include With_domain_derivation with type t := t

      include With_show with type t := t
    end

    module Hash : sig
      type t

      include With_domain_derivation with type t := t

      include With_show with type t := t
    end

    module Chain_id : sig
      type t

      include With_domain_derivation with type t := t

      include With_show with type t := t
    end

    type core_instruction =
      | Grab
      | Return
      | PushRetAddr of t
      | Apply
      | Access of int
      | Closure of t
      | EndLet

    and plain_old_data =
      | Bool of bool
      | String of string
      | Num of Z.t
      | Mutez of Z.t
      | Nil
      | Bytes of bytes
      | Address of Address.t
      | Key of Key.t
      | Hash of Hash.t
      | Key_hash of Key_hash.t
      | Chain_id of Chain_id.t

    and adt =
      | MakeRecord of int
      | RecordAccess of int
      | MakeVariant of variant_label
      | MatchVariant of (variant_label * t) list

    and operation = Eq | Add | Cons | HashKey | Or | And | Not | Pack | Unpack

    and domain_specific_operation = ChainID | Contract_opt | MakeTransaction

    and control_flow = Failwith

    and instruction =
      | Core of core_instruction
      | Plain_old_data of plain_old_data
      | Adt of adt
      | Operation of operation
      | Domain_specific_operation of domain_specific_operation
      | Control_flow of control_flow

    and t = instruction list

    include With_default_derivation with type t := t

    val equal_instruction : instruction -> instruction -> bool

    val instruction_to_string : instruction -> string

    type nonliteral_value =
      | Contract of Contract.t
      | Chain_operation of chain_operation

    and chain_operation = Transaction of Z.t * Contract.t
  end

  module Program : sig
    type t = (string * Zinc.t) list

    include With_default_derivation with type t := t
  end

  module rec Env_item : sig
    type t =
      | Z of Zinc.instruction
      | NonliteralValue of Zinc.nonliteral_value
      | Clos of Clos.t
      | Record of Stack_item.t array
      | List of Stack_item.t list
      | Variant of string * Stack_item.t

    include With_default_derivation with type t := t
  end

  and Stack_item : sig
    type t =
      | Z of Zinc.instruction
      | NonliteralValue of Zinc.nonliteral_value
      | Clos of Clos.t
      | Record of t array
      | List of t list
      | Variant of string * t
      | Marker of Zinc.t * Env_item.t list

    include With_default_derivation with type t := t
  end

  and Clos : sig
    type t = {code : Zinc.t; env : Env_item.t list}

    include With_default_derivation with type t := t
  end

  module Env : sig
    type t = Env_item.t list

    include With_default_derivation with type t := t
  end

  module Stack : sig
    type t = Stack_item.t list

    include With_default_derivation with type t := t
  end

  module Interpreter_input : sig
    type t = Zinc.t * Env.t * Stack.t

    include With_default_derivation with type t := t
  end

  module Interpreter_output : sig
    type t = Success of Env.t * Stack.t | Failure of string

    include With_default_derivation with type t := t
  end

  module Utils : sig
    val unit_record_stack : Stack_item.t

    val unit_record_env : Env_item.t
  end
end
