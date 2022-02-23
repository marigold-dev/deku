type instr =
  | ACCESS of int
  | LET
  | ENDLET
  | CLOSURE of int
  | APPLY
  | TAILAPPLY
  | RETURN
  | CONST of int
  | CONSTBYTES of bytes
  | BRANCH of int
  | BRANCHIFNOT of int
  | DUP
  | SWAP
  | MAKE_BLOCK
  | READ_FIELD
  | WRITE_FIELD
  | IADD
  | IMUL
  | IDIV
  | ISUB
  | EQINT
  | NEQINT

(* | CUSTOM *)
(* [@@deriving show] *)
module Stack = Container.Stack
module Env = Container.Env
module Memory = Container.Memory

type state = {
  (* 16-bits, 512kb *)
  stack : Stack.t;
  env : Env.t;
  (* TODO: benchmark how long it takes *)
  memory : Memory.t;
  mutable program_counter : int;
  mutable gas_counter : int; (* TODO: proper abstract type here *)
  mutable extra_args : int;
  mutable debug : bool;
}

val make_default : unit -> state

val intepret :
  ?dry_run:bool ->
  debug:bool ->
  remaining_gas:int ->
  code:instr array ->
  stack:int array ->
  state ->
  unit
