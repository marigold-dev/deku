open Zinc_utils

type address = string [@@deriving show {with_path = false}, eq, yojson]

type contract = string * address option
[@@deriving show {with_path = false}, eq, yojson]

module Zinc = struct
  type t = instruction list [@@deriving show {with_path = false}, eq, yojson]

  and instruction =
    (*
      Everything in here should be safe and trustworthy. Our assumption is that an adversary
      can create whatever zinc they want and provide it as code to the interpreter.
      The code is guaranteed
  *)
    (* ====================
       zinc core operations
       ====================
    *)
    | Grab
    | Return
    | PushRetAddr of t
    | Apply
    | Access of int
    | Closure of t
    | EndLet
    (*
     ================
     Extra operations
     ================
  *)
    (* Core types *)
    | Bool of bool
    | Eq
    | String of string
    (* math *)
    | Num of Z.t
    | Add
    (* ASTs *)
    | MakeRecord of int
    | RecordAccess of label
    | MakeVariant of variant_label
    | MatchVariant of (variant_label * t) list
    (* Lists *)
    | Nil
    | Cons
    (* Crypto *)
    | Key of string
    | HashKey
    | Hash of string
    (* serialization *)
    | Bytes of bytes
    (*
     ===========================
     tezos_specific instructions
     ===========================
  *)
    | Address of address
    | ChainID
    | Contract_opt
    | MakeTransaction
    | Mutez of Z.t
    (* Adding this to make contracts easier to interpret even though I think it's technically unecessary  *)
    | Done
    | Failwith
  [@@deriving show {with_path = false}, eq, yojson]

  (*
    Not all zinc values can be expressed directly in code as literals.
    So they're represented as a seperate type.
  *)
  type nonliteral_value = Contract of contract | Operation of operation
  [@@deriving show {with_path = false}, eq, yojson]

  and operation = Transaction of Z.t * contract (* todo: add parameter *)
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
  type t = {code : Zinc.t; env : Env_item.t list} [@@deriving show, eq, yojson]
end = struct
  type t = {code : Zinc.t; env : Env_item.t list} [@@deriving show, eq, yojson]
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
