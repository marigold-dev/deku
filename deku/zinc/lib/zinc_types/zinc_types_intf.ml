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

  module Ticket : sig
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

    module Ticket : sig
      type t

      include With_domain_derivation with type t := t

      include With_show with type t := t
    end

    module Contract : sig
      type t = {address : Address.t; entrypoint : string option}

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
      | MakeRecord of label
      | RecordAccess of label
      | MakeVariant of label
      | MatchVariant of t LMap.t

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
  end

  module Program : sig
    type t = (string * Zinc.t) list

    include With_default_derivation with type t := t
  end

  module rec Env_item : sig
    type t =
      | Z of Zinc.instruction
      | NonliteralValue of Stack_item.nonliteral_value
      | Clos of Clos.t
      | Record of Stack_item.t LMap.t
      | List of Stack_item.t list
      | Variant of Zinc_utils.label * Stack_item.t

    include With_default_derivation with type t := t
  end

  and Stack_item : sig
    type t =
      | Z of Zinc.instruction
      | NonliteralValue of nonliteral_value
      | Clos of Clos.t
      | Record of t LMap.t
      | List of t list
      | Variant of label * t
      | Marker of Zinc.t * Env_item.t list
    [@@deriving show {with_path = false}, eq, yojson]

    and nonliteral_value =
      | Contract of Zinc.Contract.t
      | Chain_operation of chain_operation
      | Ticket of Zinc.Ticket.t
    [@@deriving show {with_path = false}, eq, yojson]

    and chain_operation =
      | Transaction of t * Zinc.Contract.t (* todo: add parameter *)
    [@@deriving show {with_path = false}, eq, yojson]

    val to_string : t -> string
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

module Dummy_domain = struct
  module Address = struct
    type t = string [@@deriving eq, yojson]

    let to_string s = Printf.sprintf "\"%s\"" s

    let equal (t1 : t) (t2 : t) = equal t1 t2

    let of_string a = Some (String.sub a 1 (String.length a - 2))
  end

  module Key = struct
    type t = string [@@deriving eq, yojson]

    let to_string = Fun.id

    let of_string a = Some a
  end

  module Key_hash = struct
    type t = string [@@deriving eq, yojson]

    let to_string = Fun.id

    let of_string a = Some a
  end

  module Hash = struct
    type t = string [@@deriving eq, yojson]

    let to_string = Fun.id

    let of_string a = Some a
  end

  module Contract = struct
    type t = string * string option [@@deriving show, eq, yojson]

    let _ = pp

    let to_string x = to_yojson x |> Yojson.Safe.to_string

    let of_string x = Yojson.Safe.from_string x |> of_yojson |> Result.to_option
  end

  module Chain_id = struct
    type t = string [@@deriving show, eq, yojson]

    let _ = pp

    let to_string x = to_yojson x |> Yojson.Safe.to_string

    let of_string x = Yojson.Safe.from_string x |> of_yojson |> Result.to_option
  end

  module Ticket = struct
    type t = int64 [@@deriving show, eq, yojson]

    let _ = pp

    let to_string x = to_yojson x |> Yojson.Safe.to_string

    let of_string x = Yojson.Safe.from_string x |> of_yojson |> Result.to_option
  end
end
