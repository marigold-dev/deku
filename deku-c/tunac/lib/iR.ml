
type var = int
[@@deriving show]

type global = string
[@@deriving show]

type wasm_operation =
  | Wasm_clz
  | Wasm_ctz
  | Wasm_popcnt
  | Wasm_add
  | Wasm_sub
  | Wasm_mul
  | Wasm_div
  | Wasm_rem
  | Wasm_and
  | Wasm_or
  | Wasm_xor
  | Wasm_shl
  | Wasm_shr
  | Wasm_rotl
  | Wasm_rotr
  | Wasm_eqz
  | Wasm_eq
  | Wasm_ne
  | Wasm_lt
  | Wasm_gt
  | Wasm_le
  | Wasm_ge
[@@deriving show]

type operation =
  | Capply of string
  | Cload of int
  | Calloc of int
  | Cwasm of wasm_operation
[@@deriving show]

type expression =
  | Cconst_i32 of int32
  | Cvar of var
  | Cglobal of global
  | Cop of operation * expression list
  [@@deriving show]
  
type statement =
  | Cassign of var * expression
  | Cglobal_assign of global * expression
  | Cifthenelse of expression * statement * statement
  | Cwhile of expression * statement
  | Ccontinue
  | Cblock of statement list
  | Cstore of int * expression * expression
[@@deriving show]