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
  | BRANCHIFNOT of int
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

type state

val make_default : unit -> state

val intepret :
  ?dry_run:bool ->
  debug:bool ->
  remaining_gas:int ->
  code:instr array ->
  stack:int array ->
  state ->
  unit
