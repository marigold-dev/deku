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

  val bin_read_code : Bin_prot.Common.buf -> pos_ref:Bin_prot.Common.pos_ref -> code
  val bin_writer_code : code Bin_prot.Type_class.writer

  val bin_reader_code : code Bin_prot.Type_class.reader
  val __bin_read_code__ : Bin_prot.Common.buf -> pos_ref:Bin_prot.Common.pos_ref -> int -> code

  val bin_write_code : Bin_prot.Common.buf -> pos:int -> code -> int

  val __bin_write_code__ : Bin_prot.Common.buf -> pos:int -> code -> int

  val bin_size_code : code -> int
  val bin_shape_code : Bin_shape_lib.Bin_shape.t
  val bin_code : code Bin_prot.Type_class.t


  type value [@@deriving yojson, eq]

  val bin_read_value : Bin_prot.Common.buf -> pos_ref:Bin_prot.Common.pos_ref -> value
  val bin_writer_value : value Bin_prot.Type_class.writer

  val bin_reader_value : value Bin_prot.Type_class.reader
  val __bin_read_value__ : Bin_prot.Common.buf -> pos_ref:Bin_prot.Common.pos_ref -> int -> value

  val bin_write_value : Bin_prot.Common.buf -> pos:int -> value -> int

  val __bin_write_value__ : Bin_prot.Common.buf -> pos:int -> value -> int

  val bin_size_value : value -> int
  val bin_shape_value : Bin_shape_lib.Bin_shape.t
  val bin_value : value Bin_prot.Type_class.t

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
