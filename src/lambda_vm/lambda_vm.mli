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
end
module Gas : sig
  type t

  val make : initial_gas:int -> t
  val to_int : t -> int
  val is_empty : t -> bool

  val burn_constant : t -> unit
  val burn_log2 : t -> cardinality:int -> unit
end

(* ir *)
module Ir : sig
  type script [@@deriving yojson]
  type value [@@deriving yojson]

  val pp_value : Format.formatter -> value -> unit
end

(* compiler *)
module Compiler : sig
  type error =
    (* user program bugs *)
    [ `Out_of_gas
    | `Out_of_stack
    | `Undefined_variable ]
  [@@deriving show]

  val compile : Gas.t -> Ast.script -> (Ir.script, error) result
  val compile_value : Gas.t -> Ast.value -> (Ir.value, error) result
end

(* interpreter *)
module Interpreter : sig
  type error =
    [ `Out_of_gas
    | `Out_of_stack
    | `Over_applied_primitives
    | `Undefined_variable
    | `Value_is_not_function
    | `Value_is_not_int64
    | `Value_is_not_pair
    | `Value_is_not_zero ]
  [@@deriving show]

  type script_result = {
    storage : Ir.value;
    operations : unit;
  }
  val execute :
    Gas.t -> arg:Ir.value -> Ir.script -> (script_result, error) result
end
