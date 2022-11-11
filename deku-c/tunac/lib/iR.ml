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

type wasm_type =
  | I8
  | U8
  | I32
  | U32
[@@deriving show]

type operation =
  | Capply of string
  | Cload of int * wasm_type
  | Calloc of int
  | Cwasm of wasm_operation * wasm_type
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
  | Cfailwith of expression
[@@deriving show]

module Data = struct
  let alloc size = Cop (Calloc size, [])

  let cons var hd tl =
    Cblock
      [ Cassign (var, alloc 2)
      ; Cstore (0, Cvar var, hd)
      ; Cstore (1, Cvar var, tl) ]

  let car ?(typ = I32) expr = Cop (Cload (0, typ), [ expr ])

  let cdr ?(typ = I32) expr = Cop (Cload (1, typ), [ expr ])

  let add ?(typ = I32) a b = Cop (Cwasm (Wasm_add, typ), [ a; b ])

  let sub ?(typ = I32) a b = Cop (Cwasm (Wasm_sub, typ), [ a; b ])

  let inc x = add x (Cconst_i32 1l)

  let dec x = sub x (Cconst_i32 1l)
end