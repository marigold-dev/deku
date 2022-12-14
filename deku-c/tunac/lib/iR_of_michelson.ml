open Tezos_micheline
open IR

open Proto_alpha_utils.Memory_proto_alpha.Protocol
open Script_typed_ir

type node = (int, Michelson_v1_primitives.prim) Micheline.node

(* FIXME: Ignore the actual nodes for now *)

type error =
  | Invalid_contract_format
  | Unsupported_instruction (* of node *)
  | Unsupported_parameter_type (* of node *)
  | Unsupported_storage_type (* of node *)

exception Compilation_error of error

type function_ =
  { body : statement
  ; locals : int }

type contract =
  { main : function_
  ; lambdas : (int * function_) list
  ; static_data : bytes }

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
    [ Cassign (var, Data.car (Cglobal "__michelson_stack"))
    ; Cglobal_assign ("__michelson_stack", Data.cdr (Cglobal "__michelson_stack")) ]

let compile_push ~env expr =
  let cell = Env.alloc_local env in
  let block =
    Cblock
      [ Data.cons cell expr (Cglobal "__michelson_stack")
      ; Cglobal_assign ("__michelson_stack", Cvar cell) ]
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

let compile_dig ~env:_ n =
  Cassign (0, Cop (Capply "michelson_dig", [ Cconst_i32 n ]))

let compile_dug ~env n =
  let n = Int32.sub n 1l in
  let node = Env.alloc_local env in
  let counter = Env.alloc_local env in
  let inner_loop =
    Cblock
      [ Cassign (counter, Cconst_i32 n)
      ; Cassign (node, Data.cdr (Cglobal "__michelson_stack"))
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
      ; Cassign (head, Cglobal "__michelson_stack")
      ; Cglobal_assign ("__michelson_stack", Data.cdr (Cvar head))
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
      ; Cassign (node, Cglobal "__michelson_stack")
      ; Cwhile (Cvar counter,
         Cblock
          [ Cassign (counter, Data.dec (Cvar counter))
          ; Cassign (node, Data.cdr (Cvar node)) ] ) ]
  in
  Env.free_local env counter;
  let block =
    Cblock
      [ inner_loop
      ; Cglobal_assign ("__michelson_stack", Cvar node) ]
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
      ; Cassign (node, Cglobal "__michelson_stack")
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
      ; Cassign (node, Cglobal "__michelson_stack")
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
      ; Cstore (0, Cvar pair, Cglobal "__michelson_stack")
      ; Cstore (1, Cvar pair, Cvar node)
      ; Cglobal_assign ("__michelson_dip_stack", Cop (Cwasm (Wasm_add, I32), [ Cglobal "__michelson_dip_stack"; Cconst_i32 4l ]))
      ; Cstore (0, Cglobal "__michelson_dip_stack", Cvar pair)
      ; Cglobal_assign ("__michelson_stack", Data.cdr (Cvar node)) ]
  in
  Env.free_local env pair;
  Env.free_local env node;

  (* Deallocate and allocate again so it does not conflict with DIP's internal block *)
  let pair = Env.alloc_local env in
  let restore_stack =
    Cblock
      [ Cassign (pair, Cop (Cload (0, I32), [ Cglobal "__michelson_dip_stack" ]))
      ; Cstore (1, Data.cdr (Cvar pair), Cglobal "__michelson_stack")
      ; Cglobal_assign ("__michelson_stack", Data.car (Cvar pair)) 
      ; Cglobal_assign ("__michelson_dip_stack", Cop (Cwasm (Wasm_sub, I32), [ Cglobal "__michelson_dip_stack"; Cconst_i32 4l ] )) ]
  in

  Cblock [ inner_loop; save_stack_block; block; restore_stack ]

let lambdas = ref []
let static_data = ref Bytes.empty

let rec compile_compare: type a b. Env.t -> expression -> expression -> int -> (a, b) ty -> statement = fun env x y var typ ->
  let compare_i32 typ var x y =
    Cblock
      [ Cassign (var, Cop (Cwasm (Wasm_sub, typ), [ x; y ]))
      ; Cifthenelse
          (Cop (Cwasm (Wasm_gt, typ), [ Cvar var; Cconst_i32 0l ])
          , Cassign (var, Cconst_i32 1l)
          , Cifthenelse (Cop (Cwasm (Wasm_lt, typ), [ Cvar var; Cconst_i32 0l ])
                        , Cassign (var, Cconst_i32 (-1l))
                        , Cblock [])) ]
  in

  match typ with
  | Unit_t ->
    Cassign (var, Cconst_i32 0l)

  | Int_t ->
    compare_i32 I32 var x y

  | Pair_t (fst, snd, _, _) ->
    let a = Env.alloc_local env in
    let b = Env.alloc_local env in
    let block =
      Cblock
        [ compile_compare env (Data.car x) (Data.car y) a fst
        ; compile_compare env (Data.cdr x) (Data.cdr y) b snd
        ; Cifthenelse
            (Cop (Cwasm (Wasm_eqz, I32), [ Cvar a ])
            , Cassign (var, Cvar b)
            , Cassign (var, Cvar a)) ]
    in
    block

  | Bool_t ->
    compare_i32 I32 var x y

  | Address_t ->
    (* We agreed at some point on using ints for addresses as an index on a contact book. *)
    compare_i32 I32 var x y

  | Nat_t ->
    compare_i32 U32 var x y

  | Mutez_t ->
    compare_i32 U32 var x y

  | Timestamp_t ->
    compare_i32 U32 var x y

  | _ -> assert false

let compile_map_get env key_type map key value =
  let compare = Env.alloc_local env in
  let block =
    Cblock
      [ Cassign (value, Cconst_i32 0l)
      ; Cwhile
          (Cvar map
          , Cblock
              [ compile_compare env (Cvar key) (Data.car (Data.car (Cvar map))) compare key_type
              ; Cifthenelse
                  (Cop (Cwasm (Wasm_eqz, I32), [ Cvar compare ])
                  , Cblock
                      [ Cassign (value, Data.cdr (Data.car (Cvar map)))
                      ; Cassign (map, Cconst_i32 0l) ]
                  , Cassign (map, Data.cdr (Cvar map))) ]) ]
  in
  Env.free_local env compare;
  block

let compile_update_map env map key value =
  let head = Env.alloc_local env in
  let entry = Env.alloc_local env in
  let block =
    Cblock
    [ Cassign (entry, Cop (Calloc 2, []))
    ; Cstore (0, Cvar entry, Cvar key)
    ; Cstore (1, Cvar entry, Cvar value)
    ; Cassign (head, Cop (Calloc 2, []))
    ; Cstore (0, Cvar head, Cvar entry)
    ; Cstore (1, Cvar head, Cvar map)
    ; Cassign (map, Cvar head) ]
  in
  Env.free_local env head;
  Env.free_local env entry;
  block

let rec compile_instruction: type a b c d. Env.t -> (a, b, c, d) kinstr -> statement = fun env instr ->
  let int_operation typ op =
    let x = Env.alloc_local env in
    let y = Env.alloc_local env in
    let block =
      Cblock [ compile_pop x
             ; compile_pop y
             ; compile_push ~env (Cop (Cwasm (op, typ), [ Cvar x; Cvar y ])) ]
    in
    Env.free_local env x;
    Env.free_local env x;
    block
  in

  match instr with
  | ICar (_, k) ->
    let top = Env.alloc_local env in
    let block =
      Cblock [ compile_pop top
             ; compile_push ~env (Data.car (Cvar top)) ]
    in
    Env.free_local env top;
    Cblock [ block; compile_instruction env k ]

  | ICdr (_, k) ->
    let top = Env.alloc_local env in
    let block =
      Cblock [ compile_pop top
             ; compile_push ~env (Data.cdr (Cvar top)) ]
    in
    Env.free_local env top;
    Cblock [ block; compile_instruction env k ]

  | IUnpair (_, k) ->
    let top = Env.alloc_local env in
    let block =
      Cblock [ compile_pop top
             ; compile_push ~env (Data.cdr (Cvar top))
             ; compile_push ~env (Data.car (Cvar top)) ]
    in
    Env.free_local env top;
    Cblock [ block; compile_instruction env k ]

  | IAdd_tez (_, k) ->
    let block = int_operation I32 Wasm_add in
    Cblock [ block; compile_instruction env k ]

  | ISub_tez (_, k) ->
    let block = int_operation I32 Wasm_sub in
    Cblock [ block; compile_instruction env k ]

  | IAdd_nat (_, k) ->
    let block = int_operation U32 Wasm_add in
    Cblock [ block; compile_instruction env k ]

  | IAdd_int (_, k) ->
    let block = int_operation I32 Wasm_add in
    Cblock [ block; compile_instruction env k ]

  | ISub_int (_, k) ->
    let block = int_operation I32 Wasm_sub in
    Cblock [ block; compile_instruction env k ]

  | IMul_int (_, k) ->
    let block = int_operation I32 Wasm_mul in
    Cblock [ block; compile_instruction env k ]

  | INeg (_, k) ->
    let x = Env.alloc_local env in
    let block =
      Cblock [ compile_pop x
             ; compile_push ~env (Cop (Cwasm (Wasm_sub, I32), [ Cconst_i32 0l; Cvar x ])) ]
    in
    Env.free_local env x;
    Cblock [ block; compile_instruction env k ]

  | IEq (_, k) ->
    let p = Env.alloc_local env in
    let block =
      Cblock
        [ compile_pop p
        ; compile_push ~env (Cop (Cwasm (Wasm_sub, I32), [ Cconst_i32 0l; Cop (Cwasm (Wasm_eqz, I32), [ Cvar p ]) ])) ]
    in
    Env.free_local env p;
    Cblock [ block; compile_instruction env k ]

  | INeq (_, k) ->
    let p = Env.alloc_local env in
    let block =
      Cblock
        [ compile_pop p
        ; Cifthenelse
            (Cop (Cwasm (Wasm_eqz, I32), [ Cvar p ])
            , Cassign (p, Cconst_i32 0l)
            , Cassign (p, Cconst_i32 (-1l)))
        ; compile_push ~env (Cvar p) ]
    in
    Env.free_local env p;
    Cblock [ block; compile_instruction env k ]

  | IAbs_int (_, k) ->
    let p = Env.alloc_local env in
    let q = Env.alloc_local env in
    let block =
      Cblock
        [ compile_pop p
        ; Cassign (q, Cop (Cwasm (Wasm_shr, I32), [ Cvar p; Cconst_i32 31l ]))
        ; compile_push ~env (Cop (Cwasm (Wasm_xor, I32), [ Cop (Cwasm (Wasm_add, I32), [ Cvar p; Cvar q ]); Cvar q ])) ]
    in
    Env.free_local env p;
    Env.free_local env q;
    Cblock [ block; compile_instruction env k ]

  | IEdiv_int (_, k) ->
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
                ; Cstore (0, Cvar r, Cop (Cwasm (Wasm_div, I32), [ Cvar x; Cvar y ]))
                ; Cstore (1, Cvar r, Cop (Cwasm (Wasm_rem, I32), [ Cvar x; Cvar y ]))
                ; Cassign (x, Data.alloc 2)
                ; Cstore (0, Cvar x, Cconst_i32 1l)
                ; Cstore (1, Cvar x, Cvar r) ]
            , Cassign (x, Cconst_i32 0l))
        ; compile_push ~env (Cvar x) ]
    in
    Env.free_local env x;
    Env.free_local env y;
    Env.free_local env r;
    Cblock [ block; compile_instruction env k ]

  (* Missing arithmetic instruction: INT, ISNAT, LSL, LSR *)
  
  | IAnd (_, k) ->
    let p = Env.alloc_local env in
    let q = Env.alloc_local env in
    let block =
      Cblock
        [ compile_pop p
        ; compile_pop q
        ; compile_push ~env (Cop (Cwasm (Wasm_and, I32), [ Cvar p; Cvar q ])) ]
    in
    Env.free_local env p;
    Env.free_local env q;
    Cblock [ block; compile_instruction env k ]

  | INot (_, k) ->
    let p = Env.alloc_local env in
    let block =
      Cblock
        [ compile_pop p
        ; compile_push ~env (Cop (Cwasm (Wasm_xor, I32), [ Cvar p; Cconst_i32 0xffffffffl ])) ]
    in
    Env.free_local env p;
    Cblock [ block; compile_instruction env k ]

  | IOr (_, k) ->
    let p = Env.alloc_local env in
    let q = Env.alloc_local env in
    let block =
      Cblock
        [ compile_pop p
        ; compile_pop q
        ; compile_push ~env (Cop (Cwasm (Wasm_or, I32), [ Cvar p; Cvar q ])) ]
    in
    Env.free_local env p;
    Env.free_local env q;
    Cblock [ block; compile_instruction env k ]

  | IXor (_, k) ->
    let p = Env.alloc_local env in
    let q = Env.alloc_local env in
    let block =
      Cblock
        [ compile_pop p
        ; compile_pop q
        ; compile_push ~env (Cop (Cwasm (Wasm_xor, I32), [ Cvar p; Cvar q ])) ]
    in
    Env.free_local env p;
    Env.free_local env q;
    Cblock [ block; compile_instruction env k ]

  | IConst (_, Unit_t, (), k) ->
    let p = compile_push ~env (Cconst_i32 0l) in
    Cblock [ p; compile_instruction env k ]

  | ICons_none (_, _, k) ->
    let p = compile_push ~env (Cconst_i32 0l) in
    Cblock [ p; compile_instruction env k ]

  | INil (_, _, k) ->
    let statement = compile_push ~env (Cconst_i32 0l) in
    Cblock [ statement; compile_instruction env k ]

  | ICons_list (_, k) ->
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
    Cblock [ block; compile_instruction env k ]

  | ICons_left (_, _, k) ->
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
    Cblock [ block; compile_instruction env k ]

  | ICons_right (_, _, k) ->
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
    Cblock [ block; compile_instruction env k ]

  | ICons_pair (_, k) ->
    (* TODO: Support IComb *)
    let statement = compile_pair ~env in
    Cblock [ statement; compile_instruction env k ]

  | ICons_some (_, k) ->
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
    Cblock [ block; compile_instruction env k ]

  | IIf_left { loc = _; branch_if_left; branch_if_right; k } ->
    let p = Env.alloc_local env in
    let block =
      [ compile_pop p
      ; compile_push ~env (Cop (Cload (1, I32), [ Cvar p ])) ]
    in
    Env.free_local env p;
    let if_body =
      Cifthenelse
        (Cop (Cload (0, I32), [ Cvar p ])
        , compile_instruction env branch_if_left
        , compile_instruction env branch_if_right) 
    in
    Cblock (block @ [ if_body; compile_instruction env k ])

  | IIf { loc = _; branch_if_true; branch_if_false; k } ->
    let p = Env.alloc_local env in
    let block =
      Cblock
        [ compile_pop p
        ; Cifthenelse
           (Cvar p
           , compile_instruction env branch_if_true
           , compile_instruction env branch_if_false) ]
    in
    Env.free_local env p;
    Cblock [ block; compile_instruction env k ]

  | IIf_cons { loc = _; branch_if_cons; branch_if_nil; k } ->
    let p = Env.alloc_local env in
    let block =
      Cblock
        [ compile_pop p
        ; Cifthenelse
            (Cvar p
            , Cblock
                [ compile_push ~env (Data.cdr (Cvar p))
                 ; compile_push ~env (Data.car (Cvar p))
                 ; compile_instruction env branch_if_cons ]
            , compile_instruction env branch_if_nil) ]
    in
    Env.free_local env p;
    Cblock [ block; compile_instruction env k ]

  | IIf_none { loc = _; branch_if_some; branch_if_none; k } ->
    let p = Env.alloc_local env in
    let block =
      Cblock
        [ compile_pop p
        ; Cifthenelse
            (Cvar p
            , Cblock
                [ compile_push ~env (Data.cdr (Cvar p))
                ; compile_instruction env branch_if_some ]
            , compile_instruction env branch_if_none) ]
    in
    Env.free_local env p;
    Cblock [ block; compile_instruction env k ]

  | ISwap (_, k) ->
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
    Cblock [ block;  compile_instruction env k ]

  | IConst (_, Int_t, z, k) ->
    let value = Int64.to_int32 @@ Option.get @@ Script_int.to_int64 z in
    let statement = compile_push ~env (Cconst_i32 value) in
    Cblock [ statement; compile_instruction env k ]

  | IConst (_, Nat_t, z, k) ->
    let value = Int64.to_int32 @@ Option.get @@ Script_int.to_int64 z in
    let statement = compile_push ~env (Cconst_i32 value) in
    Cblock [ statement; compile_instruction env k ]

  | IConst (_, Mutez_t, tz, k) ->
    let value = Int64.to_int32 @@ Alpha_context.Tez.to_mutez tz in
    let statement = compile_push ~env (Cconst_i32 value) in
    Cblock [ statement; compile_instruction env k ]

  | IConst (_, String_t, v, k) ->
    let addr = Int32.of_int @@ Bytes.length !static_data in
    (* C strings will do it for now *)
    let len =
      let b = Bytes.create 4 in
      Bytes.set_int32_le b 0 (Int32.of_int (Script_string.length v));
      b
    in
    static_data :=
      Bytes.(cat !static_data (cat len (of_string @@ Script_string.to_string v ^ "\000")));
    let statement = compile_push ~env (Cconst_i32 addr) in
    Cblock [ statement; compile_instruction env k ]

  | IEmpty_map (_, _, _, k) ->
    let statement = compile_push ~env (Cconst_i32 0l) in
    Cblock [ statement; compile_instruction env k ]

  | IEmpty_set (_, _, k) ->
    let statement = compile_push ~env (Cconst_i32 0l) in
    Cblock [ statement; compile_instruction env k ]
  
  | IDig (_, n, _, k) ->
    let statement = compile_dig ~env (Int32.of_int n) in
    Cblock [ statement; compile_instruction env k ]

  | IDug (_, n, _, k) ->
    let statement = compile_dug ~env (Int32.of_int n) in
    Cblock [ statement; compile_instruction env k ]

  | IDrop (_, k) ->
    let statement = compile_drop ~env 1l in
    Cblock [ statement; compile_instruction env k ]

  | IDropn (_, n, _, k) ->
    let statement = compile_drop ~env (Int32.of_int n) in
    Cblock [ statement; compile_instruction env k ]

  | IDup (_, k) ->
    let statement = compile_dup ~env 1l in
    Cblock [ statement; compile_instruction env k ]

  | IDup_n (_, n, _, k) ->
    let statement = compile_dup ~env (Int32.of_int n) in
    Cblock [ statement; compile_instruction env k ]

  | IDipn (_, n, _, b, k) ->
    let block = compile_instruction env b in
    if n = 0 then block
    else
      let statement = compile_dip ~env (Int32.of_int n) block in
      Cblock [ statement; compile_instruction env k ]

  | IDip (_, b, _, k) ->
    let statement = compile_dip ~env 1l (compile_instruction env b) in
    Cblock [ statement; compile_instruction env k ]

  | IFailwith (_, _) ->
    let param = Env.alloc_local env in
    Cblock [ compile_pop param; Cfailwith (Cvar param) ]

  | IList_iter (_, _, b, k) ->
    let iter = Env.alloc_local env in
    let iter_body = compile_instruction env b in
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
    Cblock [ block; compile_instruction env k ]

  | ILoop (_, b, k) ->
    (* TODO: Test it *)
    let p = Env.alloc_local env in
    let body = compile_instruction env b in
    let block =
      Cblock
       [ Cassign (p, Cconst_i32 1l) 
       ; Cwhile (Cconst_i32 1l,
          Cblock
            [ compile_pop p
            ; body ]) ]
    in
    Env.free_local env p;
    Cblock [ block; compile_instruction env k ]

  | ILoop_left (_, b, k) ->
    (* TODO: Test it *)
    let p = Env.alloc_local env in
    let body = compile_instruction env b in
    let block =
      Cblock
        [ Cassign (p, Cconst_i32 1l)
        ; Cwhile (Cvar p,
          Cblock
            [ compile_pop p
            ; compile_push ~env (Data.cdr (Cvar p))
            ; Cifthenelse
                (Cop (Cload (0, I32), [ Cvar p ])
                , Cblock [ body ]
                , Cblock []) ])
            ; Cassign (p, Data.car (Cvar p)) ]
    in
    Env.free_local env p;
    Cblock [ block; compile_instruction env k ]

  | ILambda (_, Lam ({ kinstr = body; _ }, _), k) ->
    let lenv = Env.make () in
    let body = compile_instruction lenv body in
    let lambda_n = Int32.of_int (List.length !lambdas) in
    lambdas := (body, lenv) :: !lambdas;
    let p = Env.alloc_local env in
    let block =
      Cblock
        [ Cassign (p, Data.alloc 2)
        ; Cstore (0, Cvar p, Cconst_i32 0l)
        ; Cstore (1, Cvar p, Cconst_i32 lambda_n)
        ; compile_push ~env (Cvar p) ]
    in
    Env.free_local env p;
    Cblock [ block; compile_instruction env k ]

  | IApply (_, _, k) ->
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
    Cblock [ block; compile_instruction env k ]

  | IExec (_, _, k) ->
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
              ; Cstore (0, Cvar pair, Cop (Cload (2, I32), [ Cvar lambda ]))
              ; Cstore (1, Cvar pair, Cvar argument)
              ; Cassign (lambda, Data.cdr (Cvar lambda))
              ; Cassign (argument, Cvar pair) ])
        ; compile_push ~env (Cvar argument)
        ; Cassign (lambda, Data.cdr (Cvar lambda))
        ; (* Just ignore the result *)
          Cassign (argument, Cop (Capply "exec", [ Cvar lambda ])) ]
    in
    Env.free_local env argument;
    Env.free_local env lambda;
    Env.free_local env pair;
    Cblock [ block; compile_instruction env k ]

  | ICompare (_, typ, k) ->
    let x = Env.alloc_local env in
    let y = Env.alloc_local env in
    let v = Env.alloc_local env in
    let block =
      Cblock
        [ compile_pop x
        ; compile_pop y
        ; compile_compare env (Cvar x) (Cvar y) v typ
        ; compile_push ~env (Cvar v) ]
    in
    Env.free_local env v;
    Env.free_local env x;
    Env.free_local env y;
    Cblock [ block; compile_instruction env k ]

  | IGt (_, k) ->
    let p = Env.alloc_local env in
    let block =
      Cblock
        [ compile_pop p
        ; Cifthenelse
            (Cop (Cwasm (Wasm_eq, I32), [ Cvar p; Cconst_i32 1l ])
            , Cassign (p, Cconst_i32 (-1l))
            , Cassign (p, Cconst_i32 0l))
        ; compile_push ~env (Cvar p) ]
    in
    Env.free_local env p;
    Cblock [ block; compile_instruction env k ]

  | IGe (_, k) ->
    let p = Env.alloc_local env in
    let block =
      Cblock
        [ compile_pop p
        ; Cifthenelse
            (Cop (Cwasm (Wasm_lt, I32), [ Cvar p; Cconst_i32 0l ])
            , Cassign (p, Cconst_i32 0l)
            , Cassign (p, Cconst_i32 (-1l)))
        ; compile_push ~env (Cvar p) ]
    in
    Env.free_local env p;
    Cblock [ block; compile_instruction env k ]

  | ILt (_, k) ->
    let p = Env.alloc_local env in
    let block =
      Cblock
        [ compile_pop p
        ; Cifthenelse
            (Cop (Cwasm (Wasm_lt, I32), [ Cvar p; Cconst_i32 (-1l) ])
            , Cassign (p, Cconst_i32 (-1l))
            , Cassign (p, Cconst_i32 0l))
        ; compile_push ~env (Cvar p) ]
    in
    Env.free_local env p;
    Cblock [ block; compile_instruction env k ]

  | ILe (_, k) ->
    let p = Env.alloc_local env in
    let block =
      Cblock
        [ compile_pop p
        ; Cifthenelse
            (Cop (Cwasm (Wasm_gt, I32), [ Cvar p; Cconst_i32 0l ])
            , Cassign (p, Cconst_i32 0l)
            , Cassign (p, Cconst_i32 (-1l)))
        ; compile_push ~env (Cvar p) ]
    in
    Env.free_local env p;
    Cblock [ block; compile_instruction env k ]

  | ISender (_, k) ->
    let statement = compile_push ~env (Cop (Capply "sender", [])) in
    Cblock [ statement; compile_instruction env k ]

  | IAmount (_, k) ->
    let statement = compile_push ~env (Cop (Capply "amount", [])) in
    Cblock [ statement; compile_instruction env k ]

  | ITicket (_, _typ, k) ->
    let content = Env.alloc_local env in
    let amount = Env.alloc_local env in
    let ptr = Env.alloc_local env in
    let size = Env.alloc_local env in
    let block =
      Cblock
        [ compile_pop content
          (* TODO encode ticket content *)
        ; compile_pop amount
        ; compile_push ~env (Cop (Capply "ticket", [ Cvar content; Cvar amount ])) ]
    in
    Env.free_local env content;
    Env.free_local env amount;
    Env.free_local env ptr;
    Env.free_local env size;
    Cblock [ block; compile_instruction env k ]

  | IMap_get (_, k) ->
    let map = Env.alloc_local env in
    let key = Env.alloc_local env in
    let value = Env.alloc_local env in
    let map_get = compile_map_get env Int_t map key value in
    let push = compile_push ~env (Cvar value) in
    let block =
      Cblock
        [ compile_pop key
        ; compile_pop map
        ; map_get
        ; push ]
    in
    Env.free_local env map;
    Env.free_local env key;
    Env.free_local env value;
    Cblock [ block; compile_instruction env k ]

  | IMap_update (_, k) ->
    let map = Env.alloc_local env in
    let key = Env.alloc_local env in
    let value = Env.alloc_local env in
    let update = compile_update_map env map key value in
    let push = compile_push ~env (Cvar map) in
    let block =
      Cblock
        [ compile_pop key
        ; compile_pop value
        ; compile_pop map
        ; update
        ; push ]
    in
    Env.free_local env map;
    Env.free_local env key;
    Env.free_local env value;
    Cblock [ block; compile_instruction env k ]

  | ITransfer_tokens (_, k) ->
    let arg = Env.alloc_local env in
    let amount = Env.alloc_local env in
    let contract = Env.alloc_local env in
    let block =
      Cblock
        [ compile_pop arg
        ; compile_pop amount
        ; compile_pop contract
        ; compile_push ~env (Cop (Capply "transfer_tokens", [ Cvar arg; Cvar amount; Cvar contract ])) ]
    in
    Env.free_local env arg;
    Env.free_local env amount;
    Env.free_local env contract;
    Cblock [ block; compile_instruction env k ]

  | IHalt _ -> Cblock []

  | _instr -> raise (Compilation_error (Unsupported_instruction))

and compile_value_decoder: type a b. Env.t -> (a, b) ty -> int -> int -> statement = fun env typ var ptr ->
  let decode_i32 () =
    Cblock
      [ Cassign (var, Cop (Cload (0, I32), [ Cvar ptr ]))
      ; Cassign (ptr, Cop (Cwasm (Wasm_add, I32), [ Cvar ptr; Cconst_i32 4l ])) ]
  in

  let decode_list f =
    let counter = Env.alloc_local env in
    let value = Env.alloc_local env in
    let tmp = Env.alloc_local env in
    let block =
      Cblock
        [ Cassign (var, Cconst_i32 0l)
        ; Cassign (counter, Cop (Cload (0, I32), [ Cvar ptr ]))
        ; Cassign (ptr, Cop (Cwasm (Wasm_add, I32), [ Cvar ptr; Cconst_i32 4l ]))
        ; Cwhile (Cvar counter,
            Cblock
             [ f value ptr
             (* TODO: I'm not sure if I need this tmp local *)
             ; Data.cons tmp (Cvar value) (Cvar var)
             ; Cassign (var, Cvar tmp)
             ; Cassign (counter, Data.dec (Cvar counter)) ]) ]
    in
    Env.free_local env counter;
    Env.free_local env value;
    Env.free_local env tmp;
    block
  in

  match typ with
  | Bool_t -> decode_i32 ()
  | Nat_t -> decode_i32 ()
  | Int_t -> decode_i32 ()
  | Unit_t -> decode_i32 ()

  (* Let's assume contract and address are ints for now,
     they should eventually be used as strings but converted
     to integers using a contact list. *)
  | Address_t ->
    Cblock
      [ Cassign (var, Cop (Capply "lookup_address", [ Cvar ptr ]))
      ; Cassign (ptr, Data.add (Cvar ptr) (Data.add (Cconst_i32 4l) (Data.car (Cvar ptr)))) ]

  | Contract_t (_, _) -> decode_i32 ()

  | Union_t (left, right, _, _) ->
    let wrapped_value = Env.alloc_local env in
    let block =
      Cblock
        [ Cassign (var, Cop (Calloc 2, []))
        ; Cstore (0, Cvar var, Cop (Cload (0, I32), [ Cvar ptr ]))
        ; Cassign (ptr, Cop (Cwasm (Wasm_add, I32), [ Cvar ptr; Cconst_i32 4l ]))
        ; Cifthenelse (Cop (Cload (0, I32), [ Cvar var ])
            , compile_value_decoder env left wrapped_value ptr
            , compile_value_decoder env right wrapped_value ptr )
        ; Cstore (1, Cvar var, Cvar wrapped_value) ]
    in
    Env.free_local env wrapped_value;
    block

  | List_t (typ, _) ->
    decode_list (fun value ptr -> compile_value_decoder env typ value ptr)

  | Map_t (key_type, value_type, _) ->
    decode_list
      (fun value ptr ->
        let tmp = Env.alloc_local env in
        let opt = Env.alloc_local env in
        let block =
          Cblock
            [ Cassign (tmp, Data.alloc 2)
            ; compile_value_decoder env key_type value ptr
            ; Cstore (0, Cvar tmp, Cvar value)
            ; compile_value_decoder env value_type value ptr
            ; Cassign (opt, Data.alloc 2)
            ; Cstore (0, Cvar opt, Cconst_i32 1l)
            ; Cstore (1, Cvar opt, Cvar value)
            ; Cstore (1, Cvar tmp, Cvar opt)
            ; Cassign (value, Cvar tmp) ]
        in
        Env.free_local env tmp;
        Env.free_local env opt;
        block)

  | Option_t (typ, _, _) ->
    let value = Env.alloc_local env in
    let block =
      Cblock
        [ Cassign (value, Cop (Cload (0, I32), [ Cvar ptr ]))
        ; Cassign (ptr, Cop (Cwasm (Wasm_add, I32), [ Cvar ptr; Cconst_i32 4l ]))
        ; Cifthenelse (Cvar value,
            Cblock
              [ Cassign (var, Data.alloc 2)
              ; Cstore (0, Cvar var, Cvar value)
              ; compile_value_decoder env typ value ptr
              ; Cstore (1, Cvar var, Cvar value) ],
            Cassign (var, Cvar value)) ]
    in
    Env.free_local env value;
    block

  | Pair_t (a, b, _, _) ->
    let value = Env.alloc_local env in
    let block =
      Cblock
        [ Cassign (var, Data.alloc 2)
        ; compile_value_decoder env a value ptr
        ; Cstore (0, Cvar var, Cvar value)
        ; compile_value_decoder env b value ptr
        ; Cstore (1, Cvar var, Cvar value) ]
    in
    Env.free_local env value;
    block

  | String_t ->
    Cblock
      [ Cassign (var, Cvar ptr)
      ; Cassign (ptr, Data.add (Cvar ptr) (Data.add (Cconst_i32 4l) (Data.car (Cvar ptr)))) ]

  | _typ -> raise (Compilation_error (Unsupported_parameter_type))

let rec value_size: type a b. Env.t -> (a, b) ty -> expression -> int -> statement = fun env typ value size ->
  let i32 size = Cassign (size, Cconst_i32 4l) in
  let byte_seq size = Cassign (size, Data.add (Cconst_i32 4l) (Data.car value)) in
  match typ with
  | Int_t -> i32 size
  | Nat_t -> i32 size 
  | Unit_t -> i32 size
  | Mutez_t -> i32 size
  | Timestamp_t -> i32 size
  | Bool_t -> i32 size
  | Operation_t -> i32 size
  | Signature_t -> byte_seq size
  | String_t -> byte_seq size
  | Bytes_t -> byte_seq size
  | Key_hash_t -> byte_seq size
  | Key_t -> byte_seq size
  | Address_t ->
    let t = Env.alloc_local env in
    Cblock 
      [ Cassign (t, Cop (Capply "reverse_lookup_address", [ value ]))
      ; value_size env String_t (Cvar t) size ]

  | Tx_rollup_l2_address_t -> byte_seq size
  | Chain_id_t -> byte_seq size
  | Bls12_381_fr_t -> byte_seq size
  | Bls12_381_g1_t -> byte_seq size
  | Bls12_381_g2_t -> byte_seq size
  | Chest_key_t -> byte_seq size
  | Chest_t -> byte_seq size (* ?? *)

  | Pair_t (a, b, _, _) ->
    let tmp = Env.alloc_local env in
    Cblock
      [ value_size env a (Data.car value) size
      ; value_size env b (Data.cdr value) tmp
      ; Cassign (size, Data.add (Cvar size) (Cvar tmp)) ]

  | Union_t (left, right, _, _) ->
    Cblock
      [ Cifthenelse
          (Data.car value
          , value_size env left (Data.cdr value) size
          , value_size env right (Data.cdr value) size)
      ; Cassign (size, Data.add (Cvar size) (Cconst_i32 4l)) ]

  | Option_t (typ, _, _) ->
    Cifthenelse
      (value
      , Cblock
          [ value_size env typ (Data.cdr value) size
          ; Cassign (size, Data.add (Cvar size) (Cconst_i32 4l)) ]
      , i32 size)

  | List_t (typ, _) ->
    let node = Env.alloc_local env in
    let tmp = Env.alloc_local env in
    Cblock
      [ Cassign (size, Cconst_i32 4l)
      ; Cassign (node, value)
      ; Cwhile
          (Cvar node
          , Cblock
              [ value_size env typ (Cvar node) tmp
              ; Cassign (size, Data.add (Cvar size) (Cvar tmp))
              ; Cassign (node, Data.cdr (Cvar node)) ]) ]

  | Set_t (typ, _) ->
    let node = Env.alloc_local env in
    let tmp = Env.alloc_local env in
    Cblock
      [ Cassign (size, Cconst_i32 0l)
      ; Cassign (node, value)
      ; Cwhile
          (Cvar node
          , Cblock
              [ value_size env typ (Cvar node) tmp
              ; Cassign (size, Data.add (Cvar size) (Cvar tmp))
              ; Cassign (node, Data.cdr (Cvar node)) ]) ]

  | Map_t (key_type, value_type, _) ->
    let node = Env.alloc_local env in
    let tmp = Env.alloc_local env in
    Cblock
      [ Cassign (size, Cconst_i32 4l)
      ; Cassign (node, value)
      ; Cwhile
          (Cvar node
          , Cblock
              [ Cifthenelse
                  (Data.cdr (Data.car (Cvar node))
                  , Cblock
                      [ value_size env key_type (Data.car (Data.car (Cvar node))) tmp
                      ; Cassign (size, Data.add (Cvar size) (Cvar tmp))
                      ; value_size env value_type (Data.cdr (Data.cdr (Data.car (Cvar node)))) tmp
                      ; Cassign (size, Data.add (Cvar size) (Cvar tmp)) ]
                    , Cblock [])
              ; Cassign (node, Data.cdr (Cvar node)) ]) ]

  | Ticket_t _ -> i32 size

  | Sapling_transaction_t _ -> failwith "Cannot be serialized"
  | Sapling_transaction_deprecated_t _ -> failwith "Cannot be serialized"
  | Sapling_state_t _ -> failwith "Cannot be serialized"
  | Contract_t _ -> failwith "Cannot be serialized"
  | Big_map_t _ -> failwith "Cannot be serialized"
  | Lambda_t _ -> failwith "Lambdas cannot be serialized" 
  | Never_t -> failwith "Cannot serialize never"

and compile_value_encoder: type a b. bool -> Env.t -> (a, b) ty -> int -> int -> int -> statement = fun alloc env typ ptr size value ->
  let encode_i32 () =
    Cblock
    [ if alloc then Cassign (ptr, Cop (Calloc 1, [])) else Cblock []
    ; Cstore (0, Cvar ptr, Cvar value)
    ; Cassign (size, Cconst_i32 4l) ]
  in

  let encode_bytestream typ value =
    let size_statement = value_size env typ (Cvar value) size in
    let counter = Env.alloc_local env in
    let block =
      Cblock
        [ size_statement
        ; if alloc then Cassign (ptr, Cop (Calloc 0, [ Cvar size ])) else Cblock []
        ; Cassign (counter, Cconst_i32 0l)
        ; Cwhile
            (Cop (Cwasm (Wasm_lt, I32), [ Cvar counter; Cvar size ])
            , Cblock
                [ Cstore (0, Data.add (Cvar ptr) (Cvar counter), Data.car (Data.add (Cvar value) (Cvar counter)))
                ; Cassign (counter, Data.inc (Cvar counter)) ]) ]
    in
    Env.free_local env counter;
    block
  in

  match typ with
  | Int_t -> encode_i32 ()
  | Nat_t -> encode_i32 ()
  | Unit_t -> encode_i32 ()
  | Mutez_t -> encode_i32 ()
  | Timestamp_t -> encode_i32 ()
  | Bool_t -> encode_i32 ()
  | Operation_t -> encode_i32 ()

  | Signature_t -> encode_bytestream typ value
  | String_t -> encode_bytestream typ value
  | Bytes_t -> encode_bytestream typ value
  | Key_hash_t -> encode_bytestream typ value
  | Key_t -> encode_bytestream typ value
  | Address_t ->
    let value' = Env.alloc_local env in
    Cblock
      [ Cassign (value', Cop (Capply "reverse_lookup_address", [ Cvar value ]))
      ; encode_bytestream String_t value' ]

  | Tx_rollup_l2_address_t -> encode_bytestream typ value
  | Chain_id_t -> encode_bytestream typ value
  | Bls12_381_fr_t -> encode_bytestream typ value
  | Bls12_381_g1_t -> encode_bytestream typ value
  | Bls12_381_g2_t -> encode_bytestream typ value
  | Chest_key_t -> encode_bytestream typ value
  | Chest_t -> encode_bytestream typ value

  | Pair_t (a, b, _, _) ->
    let value' = Env.alloc_local env in
    let size' = Env.alloc_local env in
    let ptr' = Env.alloc_local env in
    let block =
      Cblock
      [ value_size env typ (Cvar value) size
      ; if alloc then Cassign (ptr, Cop (Calloc 0, [ Cvar size ])) else Cblock []
      ; Cassign (value', Data.car (Cvar value))
      ; compile_value_encoder false env a ptr size' value'
      ; Cassign (value', Data.cdr (Cvar value))
      ; Cassign (ptr', Data.add (Cvar ptr) (Cvar size'))
      ; compile_value_encoder false env b ptr' size' value' ]
      in
    Env.free_local env value';
    Env.free_local env size';
    Env.free_local env ptr';
    block
    
  | Union_t (left, right, _, _) ->
    let ptr' = Env.alloc_local env in
    let size' = Env.alloc_local env in
    let value' = Env.alloc_local env in
    Cblock
      [ value_size env typ (Cvar value) size
      ; if alloc then Cassign (ptr, Cop (Calloc 0, [ Cvar size ])) else Cblock []
      ; Cassign (ptr', Data.add (Cvar ptr) (Cconst_i32 4l))
      ; Cassign (value', Data.cdr (Cvar value))
      ; Cifthenelse
          (Data.car (Cvar value)
          , Cblock
          [ Cstore (0, Cvar ptr, Cconst_i32 1l)
          ; compile_value_encoder false env left ptr' size' value' ]
          , Cblock
          [ Cstore (0, Cvar ptr, Cconst_i32 0l) ])
          ; compile_value_encoder false env right ptr' size' value' ]

  | List_t (item_typ, _) ->
    let node = Env.alloc_local env in
    let ptr' = Env.alloc_local env in
    let size' = Env.alloc_local env in
    let value' = Env.alloc_local env in
    Cblock
      [ value_size env typ (Cvar value) size
      ; if alloc then Cassign (ptr, Cop (Calloc 0, [ Cvar size ])) else Cblock []
      ; Cassign (node, Cvar value)
      ; Cassign (ptr', Data.add (Cvar ptr) (Cconst_i32 4l))
      ; Cstore (0, Cvar ptr, Cconst_i32 0l)
      ; Cwhile
          (Cvar node
          , Cblock
              [ Cassign (value', Data.car (Cvar node))
              ; compile_value_encoder false env item_typ ptr' size' value'
              ; Cassign (ptr', Data.add (Cvar ptr') (Cvar size'))
              ; Cstore (0, Cvar ptr, Data.inc (Data.car (Cvar ptr)))
              ; Cassign (node, Data.cdr (Cvar node)) ])]

  | Option_t (typ', _, _) ->
    let ptr' = Env.alloc_local env in
    let size' = Env.alloc_local env in
    let value' = Env.alloc_local env in
    Cblock
      [ value_size env typ (Cvar value) size
      ; if alloc then Cassign (ptr, Cop (Calloc 0, [ Cvar size ])) else Cblock []
      ; Cifthenelse
          (Cvar value
          , Cblock
              [ Cstore (0, Cvar ptr, Cconst_i32 1l)
              ; Cassign (ptr', Data.add (Cvar ptr) (Cconst_i32 4l))
              ; Cassign (value', Data.cdr (Cvar value))
              ; compile_value_encoder false env typ' ptr' size' value' ]
          , Cstore (0, Cvar ptr, Cconst_i32 0l)) ]
      
  (* TODO *)
  | Map_t (key_type, value_type, _) ->
    let node = Env.alloc_local env in
    let ptr' = Env.alloc_local env in
    let size' = Env.alloc_local env in
    let value' = Env.alloc_local env in
    Cblock
      [ value_size env typ (Cvar value) size
      ; if alloc then Cassign (ptr, Cop (Calloc 0, [ Cvar size ])) else Cblock []
      ; Cstore (0, Cvar ptr, Cconst_i32 0l)
      ; Cassign (node, Cvar value)
      ; Cassign (ptr', Data.add (Cvar ptr) (Cconst_i32 4l))
      ; Cwhile
          (Cvar node
          , Cblock
              [ Cifthenelse
                  (Data.cdr (Data.car (Cvar node))
                  , Cblock
                      [ Cassign (value', Data.car (Data.car (Cvar node)))
                      ; compile_value_encoder false env key_type ptr' size' value'
                      ; Cassign (ptr', Data.add (Cvar ptr') (Cvar size'))
                      ; Cassign (value', Data.cdr (Data.cdr (Data.car (Cvar node))))
                      ; compile_value_encoder false env value_type ptr' size' value'
                      ; Cassign (ptr', Data.add (Cvar ptr') (Cvar size'))
                      ; Cstore (0, Cvar ptr, Data.inc (Data.car (Cvar ptr))) ]
                  , Cblock [])
              ; Cassign (node, Data.cdr (Cvar node)) ] ) ]
      
  | _typ -> raise (Compilation_error Unsupported_storage_type)

let compile_contract contract =

  let open Script_ir_translator in
  let Ex_code (Code { code = Lam ({ kinstr = code ; _ }, _); arg_type; storage_type; _ }) = contract in

  let env = Env.make () in
  let parameter = Env.alloc_local env in
  let q = Env.alloc_local env in
  let parameter_var = Env.alloc_local env in
  let param_block =
    Cblock
      [ Cassign (parameter, Cop (Calloc 0, [ Cop (Capply "parameter_size", []) ]))
      ; Cassign (q, Cop (Capply "parameter_load", [ Cvar parameter ]))
      ; Cassign (parameter_var, Cop (Calloc 2, []))
      ; compile_value_decoder env arg_type q parameter
      ; Cstore (0, Cvar parameter_var, Cvar q)
      ; compile_value_decoder env storage_type q parameter
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
      [ Cassign (value, Data.cdr (Data.car (Cglobal "__michelson_stack")))
      ; compile_value_encoder true env storage_type ptr size value
      ; Cassign (value, Cop (Capply "save_storage", [ Cvar ptr; Cvar size ])) ]
    in
    Env.free_local env ptr;
    Env.free_local env size;
    Env.free_local env value;
    block
  in

  let main =
    let body = Cblock (param_block :: compile_instruction env code :: store_block) in
    (* let body = compile_instruction env code in *)
    { body ; locals = env.max + 1 }
  in
  let lambdas =
    !lambdas
    |> List.rev
    |> List.mapi (fun idx (body, env) -> (idx, { body; locals = Env.max env + 1 }))
  in
  { main; lambdas; static_data = !static_data }

let compile_contract contract =
  try Ok (compile_contract contract)
  with Compilation_error err -> Error err
