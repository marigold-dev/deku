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
[@@deriving show]

type state

val default : unit -> state

val intepret :
  state ->
  debug:bool ->
  remaining_gas:int ->
  code:instr array ->
  stack:int array ->
  unit
