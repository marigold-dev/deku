module Gas : sig
  type t

  val make : initial_gas:int -> t
  val is_empty : t -> bool

  val burn_constant : t -> unit
  val burn_log2 : t -> cardinality:int -> unit
end
module Ast : sig
  type ident = string

  type prim =
    | Neg
    | Add
    | Sub
    | Mul
    | Div
    | Rem
    | Land
    | Lor
    | Lxor
    | Lsl
    | Lsr
    | Asr
    | Fst
    | Snd
    | Sender

  type expr =
    (* calculus *)
    | Var   of ident
    | Lam   of ident * expr
    | App   of {
        funct : expr;
        arg : expr;
      }
    (* prims *)
    | Const of int64
    | Prim  of prim
    (* branching *)
    | If    of {
        predicate : expr;
        consequent : expr;
        alternative : expr;
      }
    (* memory *)
    | Pair  of {
        first : expr;
        second : expr;
      }
  [@@deriving yojson, show]

  type value =
    | Int64 of int64
    | Pair  of value * value
  [@@deriving yojson, show]

  type script = {
    param : ident;
    code : expr;
  }
  [@@deriving yojson, show]

  val value_of_string : Gas.t -> string -> value
end

module Runtime_limits_error : sig
  type t =
    | Out_of_gas
    | Out_of_stack
  [@@deriving show]
end

module Ir : sig
  type code [@@deriving yojson, eq]
  type value [@@deriving yojson, eq]
  module Value_syntax : sig
    val int : int64 -> value
    val pair : value -> value -> value
  end

  val pp_value : Format.formatter -> value -> unit
  val pp_code : Format.formatter -> code -> unit
end

module Compiler : sig
  type compiler_error = (* user program bugs *)
    | Undefined_variable
  [@@deriving show]

  type error =
    | Compiler_error       of compiler_error
    | Runtime_limits_error of Runtime_limits_error.t
  [@@deriving show]

  val compile : Gas.t -> Ast.script -> (Ir.code, error) result
  val compile_value : Gas.t -> Ast.value -> (Ir.value, error) result
end

module Context : sig
  type t

  (* TODO: decide on whether we accept crypto primitives or michelson*)
  val make : sender:string -> source:string -> Gas.t -> t
end

module Interpreter : sig
  type interpreter_error =
    (* interpreter bugs *)
    | Undefined_variable
    | Over_applied_primitives
    (* user program bugs *)
    | Value_is_not_pair
    | Value_is_not_int64
    | Value_is_not_function
    | Value_is_not_zero
  [@@deriving show]

  type error =
    | Interpreter_error    of interpreter_error
    | Runtime_limits_error of Runtime_limits_error.t
  [@@deriving show]

  type script_result = {
    storage : Ir.value;
    operations : unit;
  }

  val execute :
    context:Context.t ->
    arg:Ir.value ->
    Ir.code ->
    (script_result, error) result
end
