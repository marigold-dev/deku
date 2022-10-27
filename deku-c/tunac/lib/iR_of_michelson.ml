open Tezos_micheline
open Micheline
open Michelson_v1_primitives
open IR

module Env = struct
  module Set = Set.Make(Int)

  type t = { mutable allocated: Set.t; mutable max: int }

  let make () = { allocated = Set.of_list [ 0 ]; max = 0 }

  let max t = t.max

  let alloc_local t =
    let rec aux reg =
      if Set.mem reg t.allocated then
        aux (reg + 1)
      else if reg > t.max then (
        t.max <- reg;
        t.allocated <- Set.add reg t.allocated;
        reg
      ) else (
        t.allocated <- Set.add reg t.allocated;
        reg
      )
    in
    aux 0

  let free_local t local =
    t.allocated <- Set.remove local t.allocated

end

let list_cons var hd tl =
  Cblock
   [ Cassign (var, Cop (Calloc 2, []))
   ; Cstore (0, Cvar var, hd)
   ; Cstore (1, Cvar var, tl) ]

let compile_car expr = Cop (Cload 0, [ expr ])

let compile_cdr expr = Cop (Cload 1, [ expr ])

let compile_pop var =
  Cblock
    [ Cassign (var, compile_car (Cglobal "stack"))
    ; Cglobal_assign ("stack", compile_cdr (Cglobal "stack")) ]

let compile_push ~env expr =
  let cell = Env.alloc_local env in
  let block =
    Cblock
      [ list_cons cell expr (Cglobal "stack")
      ; Cglobal_assign ("stack", Cvar cell) ]
  in
  Env.free_local env cell;
  block

let compile_pair ~env =
  let cell = Env.alloc_local env in
  let item = Env.alloc_local env in
  let block =
    Cblock
      [ Cassign (cell, Cop (Calloc 2, []))
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
            [ Cassign (counter, Cop (Cwasm Wasm_sub, [ Cvar counter; Cconst_i32 1l ]))
            ; Cassign (node, compile_cdr (Cvar node)) ]) ]
  in
  Env.free_local env counter;
  let a = Env.alloc_local env in
  let block =
    Cblock
      [ loop
      ; Cassign (a, compile_cdr (Cvar node))
      ; Cstore (1, Cvar node, compile_cdr (Cvar a))
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
      ; Cassign (node, compile_cdr (Cglobal "stack"))
      ; Cwhile (Cvar counter,
          Cblock
            [ Cassign (counter, Cop (Cwasm Wasm_sub, [ Cvar counter; Cconst_i32 1l ]))
            ; Cassign (node, compile_cdr (Cvar node)) ]) ]
  in
  Env.free_local env counter;
  let head = Env.alloc_local env in
  let block =
    Cblock
      [ inner_loop
      ; Cassign (head, Cglobal "stack")
      ; Cglobal_assign ("stack", compile_cdr (Cvar head))
      ; Cstore (1, Cvar head, compile_cdr (Cvar node))
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
          [ Cassign (counter, Cop (Cwasm Wasm_sub, [ Cvar counter; Cconst_i32 1l ]))
          ; Cassign (node, compile_cdr (Cvar node)) ] ) ]
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
             [ Cassign (counter, Cop (Cwasm Wasm_sub, [ Cvar counter; Cconst_i32 1l ]))
             ; Cassign (node, compile_cdr (Cvar node)) ] ) ]
  in
  Env.free_local env counter;
  let block =
    Cblock
      [ inner_loop
      ; compile_push ~env (compile_car (Cvar node)) ]
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
              [ Cassign (counter, Cop (Cwasm Wasm_sub, [ Cvar counter; Cconst_i32 1l ]))
              ; Cassign (node, compile_cdr (Cvar node)) ] ) ]
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
      ; Cglobal_assign ("stack", compile_cdr (Cvar node)) ]
  in
  Env.free_local env pair;
  Env.free_local env node;

  (* Deallocate and allocate again so it does not conflict with DIP's internal block *)
  let pair = Env.alloc_local env in
  let restore_stack =
    Cblock
      [ Cassign (pair, Cop (Cload 0, [ Cglobal "dip_stack" ]))
      ; Cstore (1, compile_cdr (Cvar pair), Cglobal "stack")
      ; Cglobal_assign ("stack", compile_car (Cvar pair)) 
      ; Cglobal_assign ("dip_stack", Cop (Cwasm Wasm_sub, [ Cglobal "dip_stack"; Cconst_i32 4l ] )) ]
  in

  Cblock [ inner_loop; save_stack_block; block; restore_stack ]

let rec compile_instruction ~env instr =
  match instr with
  | Prim (_, I_CAR, _, _) ->
    let top = Env.alloc_local env in
    let block =
      Cblock [ compile_pop top
             ; compile_push ~env (compile_car (Cvar top)) ]
    in
    Env.free_local env top;
    block

  | Prim (_, I_CDR, _, _) ->
    let top = Env.alloc_local env in
    let block =
      Cblock [ compile_pop top
             ; compile_push ~env (compile_cdr (Cvar top)) ]
    in
    Env.free_local env top;
    block

  | Prim (_, I_UNPAIR, _, _) ->
    let top = Env.alloc_local env in
    let block =
      Cblock [ compile_pop top
             ; compile_push ~env (compile_cdr (Cvar top))
             ; compile_push ~env (compile_car (Cvar top)) ]
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
  

  | Prim (_, I_NIL, _, _) ->
    compile_push ~env (Cconst_i32 0l)

  | Prim (_, I_PAIR, _, _) ->
    compile_pair ~env

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

  | Prim (_, I_PUSH, [ Prim (_, T_int, _, _); Int (_, z) ], _) ->
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

  | _ -> assert false

let rec compile_value_decoder ~env typ var ptr =
  match typ with
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

  | _ -> assert false

let compile_value_encoder ~env:_ typ ptr size value =
  match typ with
  | Prim (_, T_int, _, _) ->
    Cblock
      [ Cassign (ptr, Cop (Calloc 1, []))
      ; Cstore (0, Cvar ptr, Cvar value)
      ; Cassign (size, Cconst_i32 4l) ]

  | _ -> assert false

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
        [ Cassign (value, compile_cdr (compile_car (Cglobal "stack")))
        ; compile_value_encoder ~env storage_type ptr size value
        ; Cassign (value, Cop (Capply "save_storage", [ Cvar ptr; Cvar size ])) ]
      in
      Env.free_local env ptr;
      Env.free_local env size;
      Env.free_local env value;
      block
    in

    Cblock (param_block :: List.map (compile_instruction ~env) code @ store_block), env
  | _ -> assert false