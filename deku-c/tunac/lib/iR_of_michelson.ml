open Tezos_micheline
open Micheline
open Michelson_v1_primitives
open IR

type node = (int, Michelson_v1_primitives.prim) Micheline.node

type error =
  | Invalid_contract_format
  | Unsupported_instruction of node
  | Unsupported_parameter_type of node
  | Unsupported_storage_type of node

exception Compilation_error of error

type function_ =
  { body : statement
  ; locals : int }

type contract =
  { main : function_
  ; lambdas : (int * function_) list }

module Env = struct
  module Set = Set.Make(Int)

  type t = { mutable allocated: Set.t; mutable max: int }

  let make () = { allocated = Set.of_list [ 0 ]; max = 0 }

  let max t = t.max

  let alloc_local t =
    let rec aux reg =
      if Set.mem reg t.allocated then
        aux (reg + 1)
      else (
        t.max <- Int.max reg t.max;
        t.allocated <- Set.add reg t.allocated;
        reg
      )
    in
    aux 0

  let free_local t local =
    t.allocated <- Set.remove local t.allocated

end
let compile_pop var =
  Cblock
    [ Cassign (var, Data.car (Cglobal "stack"))
    ; Cglobal_assign ("stack", Data.cdr (Cglobal "stack")) ]

let compile_push ~env expr =
  let cell = Env.alloc_local env in
  let block =
    Cblock
      [ Data.cons cell expr (Cglobal "stack")
      ; Cglobal_assign ("stack", Cvar cell) ]
  in
  Env.free_local env cell;
  block

let compile_pair ~env =
  let cell = Env.alloc_local env in
  let item = Env.alloc_local env in
  let block =
    Cblock
      [ Cassign (cell, Data.alloc 2)
      ; compile_pop item
      ; Cstore (0, Cvar cell, Cvar item)
      ; compile_pop item
      ; Cstore (1, Cvar cell, Cvar item)
      ; compile_push ~env (Cvar cell) ]
  in
  Env.free_local env cell;
  Env.free_local env item;
  block

let compile_dig ~env n =
  let n = Int32.sub n 1l in
  let counter =  Env.alloc_local env in
  let node = Env.alloc_local env in
  let loop =
    Cblock
      [ Cassign (counter, Cconst_i32 n)
      ; Cassign (node, Cglobal "stack")
      ; Cwhile (Cvar counter,
          Cblock
            [ Cassign (counter, Data.dec (Cvar counter))
            ; Cassign (node, Data.cdr (Cvar node)) ]) ]
  in
  Env.free_local env counter;
  let a = Env.alloc_local env in
  let block =
    Cblock
      [ loop
      ; Cassign (a, Data.cdr (Cvar node))
      ; Cstore (1, Cvar node, Data.cdr (Cvar a))
      ; Cstore (1, Cvar a, Cglobal "stack")
      ; Cglobal_assign ("stack", Cvar a) ]
  in
  Env.free_local env a;
  Env.free_local env node;
  block

let compile_dug ~env n =
  let n = Int32.sub n 1l in
  let node = Env.alloc_local env in
  let counter = Env.alloc_local env in
  let inner_loop =
    Cblock
      [ Cassign (counter, Cconst_i32 n)
      ; Cassign (node, Data.cdr (Cglobal "stack"))
      ; Cwhile (Cvar counter,
          Cblock
            [ Cassign (counter, Data.dec (Cvar counter))
            ; Cassign (node, Data.cdr (Cvar node)) ]) ]
  in
  Env.free_local env counter;
  let head = Env.alloc_local env in
  let block =
    Cblock
      [ inner_loop
      ; Cassign (head, Cglobal "stack")
      ; Cglobal_assign ("stack", Data.cdr (Cvar head))
      ; Cstore (1, Cvar head, Data.cdr (Cvar node))
      ; Cstore (1, Cvar node, Cvar head) ]
  in
  Env.free_local env node;
  Env.free_local env head;
  block

let compile_drop ~env n =
  let counter = Env.alloc_local env in
  let node = Env.alloc_local env in
  let inner_loop =
    Cblock
      [ Cassign (counter, Cconst_i32 n)
      ; Cassign (node, Cglobal "stack")
      ; Cwhile (Cvar counter,
         Cblock
          [ Cassign (counter, Data.dec (Cvar counter))
          ; Cassign (node, Data.cdr (Cvar node)) ] ) ]
  in
  Env.free_local env counter;
  let block =
    Cblock
      [ inner_loop
      ; Cglobal_assign ("stack", Cvar node) ]
  in
  Env.free_local env node;
  block

let compile_dup ~env n =
  let n = Int32.sub n 1l in
  let counter = Env.alloc_local env in
  let node = Env.alloc_local env in
  let inner_loop =
    Cblock
      [ Cassign (counter, Cconst_i32 n)
      ; Cassign (node, Cglobal "stack")
      ; Cwhile (Cvar counter
          , Cblock
             [ Cassign (counter, Data.dec (Cvar counter))
             ; Cassign (node, Data.cdr (Cvar node)) ] ) ]
  in
  Env.free_local env counter;
  let block =
    Cblock
      [ inner_loop
      ; compile_push ~env (Data.car (Cvar node)) ]
  in
  Env.free_local env node;
  block

let compile_dip ~env n block =
  let n = Int32.sub n 1l in
  let node = Env.alloc_local env in
  let counter = Env.alloc_local env in
  let inner_loop =
    Cblock
      [ Cassign (counter, Cconst_i32 n)
      ; Cassign (node, Cglobal "stack")
      ; Cwhile (Cvar counter
          , Cblock
              [ Cassign (counter, Data.dec (Cvar counter))
              ; Cassign (node, Data.cdr (Cvar node)) ] ) ]
  in
  Env.free_local env counter;

  let pair = Env.alloc_local env in
  let save_stack_block =
    Cblock
      [ Cassign (pair, Cop (Calloc 2, []))
      ; Cstore (0, Cvar pair, Cglobal "stack")
      ; Cstore (1, Cvar pair, Cvar node)
      ; Cglobal_assign ("dip_stack", Cop (Cwasm Wasm_add, [ Cglobal "dip_stack"; Cconst_i32 4l ]))
      ; Cstore (0, Cglobal "dip_stack", Cvar pair)
      ; Cglobal_assign ("stack", Data.cdr (Cvar node)) ]
  in
  Env.free_local env pair;
  Env.free_local env node;

  (* Deallocate and allocate again so it does not conflict with DIP's internal block *)
  let pair = Env.alloc_local env in
  let restore_stack =
    Cblock
      [ Cassign (pair, Cop (Cload 0, [ Cglobal "dip_stack" ]))
      ; Cstore (1, Data.cdr (Cvar pair), Cglobal "stack")
      ; Cglobal_assign ("stack", Data.car (Cvar pair)) 
      ; Cglobal_assign ("dip_stack", Cop (Cwasm Wasm_sub, [ Cglobal "dip_stack"; Cconst_i32 4l ] )) ]
  in

  Cblock [ inner_loop; save_stack_block; block; restore_stack ]

let lambdas = ref []

let rec compile_instruction ~env instr =
  match instr with
  | Prim (_, I_CAR, _, _) ->
    let top = Env.alloc_local env in
    let block =
      Cblock [ compile_pop top
             ; compile_push ~env (Data.car (Cvar top)) ]
    in
    Env.free_local env top;
    block

  | Prim (_, I_CDR, _, _) ->
    let top = Env.alloc_local env in
    let block =
      Cblock [ compile_pop top
             ; compile_push ~env (Data.cdr (Cvar top)) ]
    in
    Env.free_local env top;
    block

  | Prim (_, I_UNPAIR, _, _) ->
    let top = Env.alloc_local env in
    let block =
      Cblock [ compile_pop top
             ; compile_push ~env (Data.cdr (Cvar top))
             ; compile_push ~env (Data.car (Cvar top)) ]
    in
    Env.free_local env top;
    block

  | Prim (_, I_ADD, _, _) ->
    let x = Env.alloc_local env in
    let y = Env.alloc_local env in
    let block =
      Cblock [ compile_pop x
             ; compile_pop y
             ; compile_push ~env (Cop (Cwasm Wasm_add, [ Cvar x; Cvar y ])) ]
    in
    Env.free_local env x;
    Env.free_local env y;
    block

  | Prim (_, I_SUB, _, _) ->
    let x = Env.alloc_local env in
    let y = Env.alloc_local env in
    let block =
      Cblock [ compile_pop x
             ; compile_pop y
             ; compile_push ~env (Cop (Cwasm Wasm_sub, [ Cvar x; Cvar y ])) ]
    in
    Env.free_local env x;
    Env.free_local env y;
    block

  | Prim (_, I_MUL, _, _) ->
    let x = Env.alloc_local env in
    let y = Env.alloc_local env in
    let block =
      Cblock [ compile_pop x
              ; compile_pop y
              ; compile_push ~env (Cop (Cwasm Wasm_mul, [ Cvar x; Cvar y ])) ]
    in
    Env.free_local env x;
    Env.free_local env y;
    block

  | Prim (_, I_NEG, _, _) ->
    let x = Env.alloc_local env in
    let block =
      Cblock [ compile_pop x
              ; compile_push ~env (Cop (Cwasm Wasm_sub, [ Cconst_i32 0l; Cvar x ])) ]
    in
    Env.free_local env x;
    block

  | Prim (_, I_EQ, _, _) ->
    let p = Env.alloc_local env in
    let block =
      Cblock
        [ compile_pop p
        ; compile_push ~env (Cop (Cwasm Wasm_sub, [ Cconst_i32 0l; Cop (Cwasm Wasm_eqz, [ Cvar p ]) ])) ]
    in
    Env.free_local env p;
    block  

  | Prim (_, I_ABS, _, _) ->
    let p = Env.alloc_local env in
    let q = Env.alloc_local env in
    let block =
      Cblock
        [ compile_pop p
        ; Cassign (q, Cop (Cwasm Wasm_shr, [ Cvar p; Cconst_i32 31l ]))
        ; compile_push ~env (Cop (Cwasm Wasm_xor, [ Cop (Cwasm Wasm_add, [ Cvar p; Cvar q ]); Cvar q ])) ]
    in
    Env.free_local env p;
    Env.free_local env q;
    block

  | Prim (_, I_EDIV, _, _) ->
    let x = Env.alloc_local env in
    let y = Env.alloc_local env in
    let r = Env.alloc_local env in
    let block =
      Cblock
        [ compile_pop x
        ; compile_pop y
        ; Cifthenelse
            (Cvar y
            , Cblock
                [ Cassign (r, Data.alloc 2)
                ; Cstore (0, Cvar r, Cop (Cwasm Wasm_div, [ Cvar x; Cvar y ]))
                ; Cstore (1, Cvar r, Cop (Cwasm Wasm_rem, [ Cvar x; Cvar y ]))
                ; Cassign (x, Data.alloc 2)
                ; Cstore (0, Cvar x, Cconst_i32 1l)
                ; Cstore (1, Cvar x, Cvar r) ]
            , Cassign (x, Cconst_i32 0l))
        ; compile_push ~env (Cvar x) ]
    in
    Env.free_local env x;
    Env.free_local env y;
    Env.free_local env r;
    block

  (* Missing arithmetic instruction: COMPARE, GE, GT, INT, ISNAT, LE, LSL, LSR, LT, NEQ *)
  
  | Prim (_, I_AND, _, _) ->
    let p = Env.alloc_local env in
    let q = Env.alloc_local env in
    let block =
      Cblock
        [ compile_pop p
        ; compile_pop q
        ; compile_push ~env (Cop (Cwasm Wasm_and, [ Cvar p; Cvar q ])) ]
    in
    Env.free_local env p;
    Env.free_local env q;
    block

  | Prim (_, I_NOT, _, _) ->
    let p = Env.alloc_local env in
    let block =
      Cblock
        [ compile_pop p
        ; compile_push ~env (Cop (Cwasm Wasm_xor, [ Cvar p; Cconst_i32 0xffffffffl ]))]
    in
    Env.free_local env p;
    block

  | Prim (_, I_OR, _, _) ->
    let p = Env.alloc_local env in
    let q = Env.alloc_local env in
    let block =
      Cblock
        [ compile_pop p
        ; compile_pop q
        ; compile_push ~env (Cop (Cwasm Wasm_or, [ Cvar p; Cvar q ])) ]
    in
    Env.free_local env p;
    Env.free_local env q;
    block

  | Prim (_, I_XOR, _, _) ->
    let p = Env.alloc_local env in
    let q = Env.alloc_local env in
    let block =
      Cblock
        [ compile_pop p
        ; compile_pop q
        ; compile_push ~env (Cop (Cwasm Wasm_xor, [ Cvar p; Cvar q ])) ]
    in
    Env.free_local env p;
    Env.free_local env q;
    block

  | Prim (_, I_UNIT, _, _)
  | Prim (_, I_NONE, _, _)
  | Prim (_, I_NIL, _, _) ->
    compile_push ~env (Cconst_i32 0l)

  | Prim (_, I_CONS, _, _) ->
    let value = Env.alloc_local env in
    let list = Env.alloc_local env in
    let new_list = Env.alloc_local env in
    let block =
      Cblock
        [ compile_pop value
        ; compile_pop list
        ; Cassign (new_list, Data.alloc 2)
        ; Cstore (0, Cvar new_list, Cvar value)
        ; Cstore (1, Cvar new_list, Cvar list)
        ; compile_push ~env (Cvar new_list) ]
    in
    Env.free_local env value;
    Env.free_local env list;
    Env.free_local env new_list;
    block

  | Prim (_, I_LEFT, _, _) ->
    let value = Env.alloc_local env in
    let p = Env.alloc_local env in
    let block =
      Cblock
        [ compile_pop value
        ; Cassign (p, Data.alloc 2)
        ; Cstore (0, Cvar p, Cconst_i32 1l)
        ; Cstore (1, Cvar p, Cvar value)
        ; compile_push ~env (Cvar p) ]
    in
    Env.free_local env value;
    Env.free_local env p;
    block

  | Prim (_, I_RIGHT, _, _) ->
    let value = Env.alloc_local env in
    let p = Env.alloc_local env in
    let block =
      Cblock
        [ compile_pop value
        ; Cassign (p, Data.alloc 2)
        ; Cstore (0, Cvar p, Cconst_i32 0l)
        ; Cstore (1, Cvar p, Cvar value)
        ; compile_push ~env (Cvar p) ]
    in
    Env.free_local env value;
    Env.free_local env p;
    block

  | Prim (_, I_PAIR, _, _) ->
    compile_pair ~env

  | Prim (_, I_SOME, _, _) ->
    (* TODO: I actually think that optionals may have only one cell allocated *)
    let p = Env.alloc_local env in
    let value = Env.alloc_local env in
    let block =
      Cblock
        [ compile_pop p
        ; Cassign (value, Data.alloc 2)
        ; Cstore (0, Cvar value, Cconst_i32 1l)
        ; Cstore (1, Cvar value, Cvar p)
        ; compile_push ~env (Cvar value) ]
    in
    Env.free_local env p;
    Env.free_local env value;
    block

  | Prim (_, I_IF_LEFT, [ Seq (_, left_branch); Seq (_, right_branch) ], _) ->
    let p = Env.alloc_local env in
    let block =
      Cblock [ compile_pop p
             ; compile_push ~env (Cop (Cload 1, [ Cvar p ]))
             ; Cifthenelse
                (Cop (Cload 0, [ Cvar p ])
                , Cblock (List.map (compile_instruction ~env) left_branch)
                , Cblock (List.map (compile_instruction ~env) right_branch)) ]
    in
    Env.free_local env p;
    block

  | Prim (_, I_IF, [ Seq (_, branch_if); Seq (_, branch_else) ], _) ->
    let p = Env.alloc_local env in
    let block =
      Cblock
        [ compile_pop p
        ; Cifthenelse
           (Cvar p
           , Cblock (List.map (compile_instruction ~env) branch_if)
           , Cblock (List.map (compile_instruction ~env) branch_else)) ]
    in
    Env.free_local env p;
    block

  | Prim (_, I_IF_CONS, [ Seq (_, branch_cons); Seq (_, branch_nil) ], _) ->
    let p = Env.alloc_local env in
    let block =
      Cblock
        [ compile_pop p
        ; Cifthenelse
            (Cvar p
            , Cblock
                ([ compile_push ~env (Data.cdr (Cvar p))
                 ; compile_push ~env (Data.car (Cvar p)) ]
                  @ List.map (compile_instruction ~env) branch_cons)
            , Cblock (List.map (compile_instruction ~env) branch_nil)) ]
    in
    Env.free_local env p;
    block

  | Prim (_, I_IF_NONE, [ Seq (_, branch_none); Seq (_, branch_some) ], _) ->
    let p = Env.alloc_local env in
    let block =
      Cblock
        [ compile_pop p
        ; Cifthenelse
            (Cvar p
            , Cblock
                (compile_push ~env (Data.cdr (Cvar p))
                 :: List.map (compile_instruction ~env) branch_some)
            , Cblock (List.map (compile_instruction ~env) branch_none)) ]
    in
    Env.free_local env p;
    block

  | Prim (_, I_SWAP, _, _) ->
    let fst = Env.alloc_local env in
    let snd = Env.alloc_local env in
    let block =
      Cblock [ compile_pop fst
             ; compile_pop snd
             ; compile_push ~env (Cvar fst)
             ; compile_push ~env (Cvar snd) ]
    in
    Env.free_local env fst;
    Env.free_local env snd;
    block

  | Prim (_, I_PUSH, [ _; Int (_, z) ], _) ->
    let value = Z.to_int32 z in
    compile_push ~env (Cconst_i32 value)

  | Prim (_, I_DIG, [ Int (_, n) ], _) ->
    let n = Z.to_int32 n in
    compile_dig ~env n

  | Prim (_, I_DUG, [ Int (_, n) ], _) ->
    let n = Z.to_int32 n in
    compile_dug ~env n

  | Prim (_, I_DROP, [], _) ->
    compile_drop ~env 1l

  | Prim (_, I_DROP, [ Int (_, n) ], _) ->
    let n = Z.to_int32 n in
    compile_drop ~env n

  | Prim (_, I_DUP, [], _) ->
    compile_dup ~env 1l

  | Prim (_, I_DUP, [ Int (_, n) ], _) ->
    compile_dup ~env (Z.to_int32 n)

  | Prim (_, I_DIP, [ Int (_, n); Seq (_, instr) ], _) ->
    let n = Z.to_int32 n in
    let block = Cblock (List.map (compile_instruction ~env) instr) in
    if n = 0l then block
    else compile_dip ~env n block

  | Prim (_, I_DIP, [ Seq (_, instr) ], _) ->
    compile_dip ~env 1l (Cblock (List.map (compile_instruction ~env) instr))

  | Prim (_, I_FAILWITH, _, _) ->
    let param = Env.alloc_local env in
    Cblock [ compile_pop param; Cfailwith (Cvar param) ]

  | Prim (_, I_ITER, [ Seq (_, body) ], _) ->
    let iter = Env.alloc_local env in
    let iter_body = Cblock (List.map (compile_instruction ~env) body) in
    let block =
      Cblock
        [ compile_pop iter
        ; Cwhile (Cvar iter,
            Cblock
              [ compile_push ~env (Data.car (Cvar iter))
              ; iter_body
              ; Cassign (iter, (Data.cdr (Cvar iter))) ]) ]
    in
    Env.free_local env iter;
    block

  | Prim (_, I_LOOP, [ Seq (_, body) ], _) ->
    (* TODO: Test it *)
    let p = Env.alloc_local env in
    let body = Cblock (List.map (compile_instruction ~env) body) in
    let block =
      Cblock
       [ Cassign (p, Cconst_i32 1l) 
       ; Cwhile (Cconst_i32 1l,
          Cblock
            [ compile_pop p
            ; body ]) ]
    in
    Env.free_local env p;
    block

  | Prim (_, I_LOOP_LEFT, [ Seq (_, body) ], _) ->
    (* TODO: Test it *)
    let p = Env.alloc_local env in
    let body = Cblock (List.map (compile_instruction ~env) body) in
    Cblock
      [ Cassign (p, Cconst_i32 1l)
      ; Cwhile (Cvar p,
         Cblock
          [ compile_pop p
          ; compile_push ~env (Data.cdr (Cvar p))
          ; Cifthenelse
              (Cop (Cload 0, [ Cvar p ])
              , Cblock [ body ]
              , Cblock []) ])
          ; Cassign (p, Data.car (Cvar p)) ]

  | Prim (_, I_LAMBDA, [ _; _; Seq (_, body) ], _) ->
    let lenv = Env.make () in
    let body = Cblock (List.map (compile_instruction ~env:lenv) body) in
    let lambda_n = Int32.of_int (List.length !lambdas) in
    lambdas := (body, env) :: !lambdas;
    let p = Env.alloc_local env in
    let block =
      Cblock
        [ Cassign (p, Data.alloc 2)
        ; Cstore (0, Cvar p, Cconst_i32 0l)
        ; Cstore (1, Cvar p, Cconst_i32 lambda_n)
        ; compile_push ~env (Cvar p) ]
    in
    Env.free_local env p;
    block

  | Prim (_, I_APPLY, _, _) ->
    let top = Env.alloc_local env in
    let value = Env.alloc_local env in
    let block =
      Cblock
        [ Cassign (value, Data.alloc 3)
        ; Cstore (0, Cvar value, Cconst_i32 1l)        
        ; compile_pop top
        ; Cstore (2, Cvar value, Cvar top)
        ; compile_pop top
        ; Cstore (1, Cvar value, Cvar top) ]
    in
    Env.free_local env value;
    Env.free_local env top;
    block

  | Prim (_, I_EXEC, _, _) ->
    (* Layout:
        0x0 <lambda index>
        0x1 <lambda pointer> <argument> *)
    let argument = Env.alloc_local env in
    let lambda = Env.alloc_local env in
    let pair = Env.alloc_local env in
    let block =
      Cblock
        [ compile_pop argument
        ; compile_pop lambda
        ; Cwhile (Data.car (Cvar lambda),
            Cblock
              [ Cassign (pair, Data.alloc 2)
              ; Cstore (0, Cvar pair, Cop (Cload 2, [ Cvar lambda ]))
              ; Cstore (1, Cvar pair, Cvar argument)
              ; Cassign (lambda, Data.cdr (Cvar lambda))
              ; Cassign (argument, Cvar pair) ])
        ; compile_push ~env (Cvar argument)
        ; Cassign (-1, Cop (Capply "exec", [ Cvar lambda ])) ]
    in
    Env.free_local env argument;
    Env.free_local env lambda;
    Env.free_local env pair;
    block

  | instr -> raise (Compilation_error (Unsupported_instruction instr))

let rec compile_value_decoder ~env typ var ptr =
  match typ with
  | Prim (_, T_bool, _, _)
  | Prim (_, T_nat, _, _)
  | Prim (_, T_int, _, _)
  | Prim (_, T_unit, _, _) ->
    Cblock
      [ Cassign (var, Cop (Cload 0, [ Cvar ptr ]))
      ; Cassign (ptr, Cop (Cwasm Wasm_add, [ Cvar ptr; Cconst_i32 4l ])) ]

  | Prim (_, T_or, [ left; right ], _) ->
    let wrapped_value = Env.alloc_local env in
    let block =
      Cblock
        [ Cassign (var, Cop (Calloc 2, []))
        ; Cstore (0, Cvar var, Cop (Cload 0, [ Cvar ptr ]))
        ; Cassign (ptr, Cop (Cwasm Wasm_add, [ Cvar ptr; Cconst_i32 4l ]))
        ; Cifthenelse (Cop (Cload 0, [ Cvar var ])
            , compile_value_decoder ~env left wrapped_value ptr
            , compile_value_decoder ~env right wrapped_value ptr )
        ; Cstore (1, Cvar var, Cvar wrapped_value) ]
    in
    Env.free_local env wrapped_value;
    block

  | Prim (_, T_list, [ typ ], _) ->
    let counter = Env.alloc_local env in
    let value = Env.alloc_local env in
    let tmp = Env.alloc_local env in
    let block =
      Cblock
        [ Cassign (var, Cconst_i32 0l)
        ; Cassign (counter, Cop (Cload 0, [ Cvar ptr ]))
        ; Cassign (ptr, Cop (Cwasm Wasm_add, [ Cvar ptr; Cconst_i32 4l ]))
        ; Cwhile (Cvar counter,
            Cblock
             [ compile_value_decoder ~env typ value ptr
             (* TODO: I'm not sure if I need this tmp local *)
             ; Data.cons tmp (Cvar value) (Cvar var)
             ; Cassign (var, Cvar tmp)
             ; Cassign (counter, Data.dec (Cvar counter)) ]) ]
    in
    Env.free_local env counter;
    Env.free_local env value;
    Env.free_local env tmp;
    block

  | Prim (_, T_option, [ typ ], _) ->
    let value = Env.alloc_local env in
    Cblock
      [ Cassign (value, Cop (Cload 0, [ Cvar ptr ]))
      ; Cassign (ptr, Cop (Cwasm Wasm_add, [ Cvar ptr; Cconst_i32 4l ]))
      ; Cifthenelse (Cvar value,
          Cblock
            [ Cassign (var, Data.alloc 2)
            ; Cstore (0, Cvar var, Cvar value)
            ; compile_value_decoder ~env typ value ptr
            ; Cstore (1, Cvar var, Cvar value) ],
          Cassign (var, Cvar value)) ]

  | typ -> raise (Compilation_error (Unsupported_parameter_type typ))

let compile_value_encoder ~env:_ typ ptr size value =
  match typ with
  | Prim (_, T_int, _, _) ->
    Cblock
      [ Cassign (ptr, Cop (Calloc 1, []))
      ; Cstore (0, Cvar ptr, Cvar value)
      ; Cassign (size, Cconst_i32 4l) ]

  | typ -> raise (Compilation_error (Unsupported_storage_type typ))

let compile_contract contract =
  let env = Env.make () in
  match contract with
  | Seq (_
      , [ Prim (_, K_parameter, [ parameter_type ], _)
        ; Prim (_, K_storage, [ storage_type ], _)
        ; Prim (_, K_code, [ Seq (_, code) ], _) ]) ->
    let parameter = Env.alloc_local env in
    let q = Env.alloc_local env in
    let parameter_var = Env.alloc_local env in
    let param_block =
      Cblock
        [ Cassign (parameter, Cop (Calloc 0, [ Cop (Capply "parameter_size", []) ]))
        ; Cassign (q, Cop (Capply "parameter_load", [ Cvar parameter ]))
        ; Cassign (parameter_var, Cop (Calloc 2, []))
        ; compile_value_decoder ~env parameter_type q parameter
        ; Cstore (0, Cvar parameter_var, Cvar q)
        ; compile_value_decoder ~env storage_type q parameter
        ; Cstore (1, Cvar parameter_var, Cvar q)
        ; compile_push ~env (Cvar parameter_var) ]
    in
    Env.free_local env parameter;
    Env.free_local env q;
    Env.free_local env parameter_var;

    let store_block =
      let ptr = Env.alloc_local env in
      let size = Env.alloc_local env in
      let value = Env.alloc_local env in
      let block =
        [ Cassign (value, Data.cdr (Data.car (Cglobal "stack")))
        ; compile_value_encoder ~env storage_type ptr size value
        ; Cassign (value, Cop (Capply "save_storage", [ Cvar ptr; Cvar size ])) ]
      in
      Env.free_local env ptr;
      Env.free_local env size;
      Env.free_local env value;
      block
    in

    let main =
      { body = Cblock (param_block :: List.map (compile_instruction ~env) code @ store_block)
      ; locals = Env.max env + 1 }
    in
    let lambdas =
      !lambdas
      |> List.rev
      |> List.mapi (fun idx (body, env) -> (idx, { body; locals = Env.max env + 1 }))
    in
    { main; lambdas }

  | _ -> raise (Compilation_error Invalid_contract_format)

let compile_contract contract =
  try Ok (compile_contract contract)
  with Compilation_error err -> Error err
