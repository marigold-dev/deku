open Zinc_utils

type address = string [@@deriving show {with_path = false}, eq, yojson]

type contract = string * address option
[@@deriving show {with_path = false}, eq, yojson]

type 'a zinc_instruction =
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
  | PushRetAddr of 'a zinc
  | Apply
  | Access of int
  | Closure of 'a zinc
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
  | MakeRecord of label list
  | RecordAccess of label
  | MakeVariant of label
  | MatchVariant of (label * 'a zinc) list
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
  (* Extensions *)
  | Extensions of 'a
[@@deriving show {with_path = false}, eq, yojson, map]

and 'a zinc = 'a zinc_instruction list
[@@deriving show {with_path = false}, eq, yojson, map]

type zinc_extension_constructors =
  (*
      Need to come up with a better name than zinc_extension_constructors,
      it's for zinc "instructions" that can't be passed as code.
      (Instead they can only be present in the stack or environment)
  *)
  | Contract of contract
  | Operation of operation
[@@deriving show {with_path = false}, eq, yojson]

and operation = Transaction of Z.t * contract (* todo: add parameter *)
[@@deriving show {with_path = false}, eq, yojson]

type zinc_instruction_code = Nothing.t zinc_instruction
[@@deriving show {with_path = false}, eq, yojson]

type zinc_instruction_extended = zinc_extension_constructors zinc_instruction
[@@deriving show {with_path = false}, eq, yojson]

type zinc_code = Nothing.t zinc
[@@deriving show {with_path = false}, eq, yojson]

type zinc_extended = zinc_extension_constructors zinc
[@@deriving show {with_path = false}, eq, yojson]

type program = (string * Nothing.t zinc) list
[@@deriving show {with_path = false}, eq, yojson]

module rec Env_item : sig
  type t =
    | Z of zinc_instruction_extended
    | Clos of Clos.t
    | Record of Stack_item.t LMap.t
    | Variant of label * Stack_item.t
  [@@deriving show, eq, yojson]
end = struct
  type t =
    | Z of zinc_instruction_extended
    | Clos of Clos.t
    | Record of Stack_item.t LMap.t
    | Variant of label * Stack_item.t
  [@@deriving show, eq, yojson]
end

and Stack_item : sig
  type t =
    | Z of zinc_instruction_extended
    | Clos of Clos.t
    | Record of t LMap.t
    | Variant of label * t
    | Marker of zinc_extended * Env_item.t list
  [@@deriving show, eq, yojson]
end = struct
  type t =
    | Z of zinc_instruction_extended
    | Clos of Clos.t
    | Record of t LMap.t
    | Variant of label * t
    | Marker of zinc_extended * Env_item.t list
  [@@deriving show, eq, yojson]
end

and Clos : sig
  type t = {code : zinc_extended; env : Env_item.t list}
  [@@deriving show, eq, yojson]
end = struct
  type t = {code : zinc_extended; env : Env_item.t list}
  [@@deriving show, eq, yojson]
end

type env = Env_item.t list [@@deriving show, eq, yojson]

type stack = Stack_item.t list [@@deriving show, eq, yojson]

type interpreter_input = zinc_code * env * stack [@@deriving show, eq, yojson]

module Interpreter_output = struct
  type t = Success of env * stack | Failure of string
  [@@deriving show, eq, yojson]
end

let generalize_zinc_instruction :
      'a. zinc_instruction_code -> 'a zinc_instruction =
  map_zinc_instruction Nothing.unreachable_code

and generalize_zinc : 'a. zinc_code -> 'a zinc =
  map_zinc Nothing.unreachable_code

type interpreter_context = {get_contract_opt : address -> contract option}
(* TODO: get_contract_opt needs to accept a type too *)

module Utils = struct
  let unit_record_stack = Stack_item.Record LMap.empty

  let unit_record_env = Env_item.Record LMap.empty
end
