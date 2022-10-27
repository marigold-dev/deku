module FuncType = struct
  open Wasm

  let ( -%> ) a b = Types.FuncType (a, b)
  let i64 = Wasm.Types.(NumType I64Type)
  let i32 = Wasm.Types.(NumType I32Type)
  let func (name, typ, f) = (name, Instance.ExternFunc (Func.alloc_host typ f))
end

exception Type_error

module Vec = struct
  module Table = Hashtbl.Make (struct
    type t = Int64.t

    let hash = Hashtbl.hash
    let equal = Int64.equal
  end)

  type t = { mutable counter : Int64.t; contents : Value.t Table.t }

  let empty = { counter = Int64.min_int; contents = Table.create 4000 }

  let alloc t item =
    let counter = t.counter in
    Table.replace t.contents counter item;
    t.counter <- Int64.(add counter 1L);
    counter

  (* let get t idx =
     match Table.find_opt t.contents idx with
     | Some x -> x
     | None -> raise Type_error *)

  let get_and_remove t idx =
    let value = Table.find_opt t.contents idx in
    match value with
    | Some x ->
        Table.remove t.contents idx;
        x
    | None -> raise Type_error

  let dup t idx =
    let value = Table.find_opt t.contents idx in
    match value with Some x -> alloc t x | None -> raise Type_error
end

open FuncType
open Effects
open Value

module Syntax = struct
  let ( %-< ) a f = f a
  let pair = function Pair (x, y) -> (x, y) | _ -> raise Type_error
  let int = function Int x -> x | _ -> raise Type_error
  let bool = function Bool x -> x | _ -> raise Type_error
  let string = function String x -> x | _ -> raise Type_error
  let option = function Option x -> x | _ -> raise Type_error
  let union = function Union x -> x | _ -> raise Type_error
  let wasm_i64 a = [ Wasm.Values.Num (I64 a) ]
  let wasm_i64' a = Wasm.Values.Num (I64 a)
  let wasm_i32 a = [ Wasm.Values.Num (I32 a) ]
end

let to_utf8 s = Wasm.Utf8.decode s

open Syntax

let vec = Vec.empty

let reset () =
  Vec.Table.clear vec.contents;
  vec.counter <- Int64.min_int

let car =
  ( to_utf8 "car",
    [ i64 ] -%> [ i64 ],
    fun inst args ->
      Wasm.Instance.burn_gas !inst 100L;
      match args with
      | Wasm.Values.[ Num (I64 x) ] ->
          let x, _ = Vec.get_and_remove vec x %-< pair in
          let result = Vec.alloc vec x in
          wasm_i64 result
      | _ -> raise Type_error )

let some =
  ( to_utf8 "some",
    [ i64 ] -%> [ i64 ],
    fun inst args ->
      Wasm.Instance.burn_gas !inst 100L;
      match args with
      | Wasm.Values.[ Num (I64 x) ] ->
          let x = Vec.get_and_remove vec x in
          let result = Vec.alloc vec (Option (Some x)) in
          wasm_i64 result
      | _ -> raise Type_error )

let left =
  ( to_utf8 "left",
    [ i64 ] -%> [ i64 ],
    fun inst args ->
      Wasm.Instance.burn_gas !inst 100L;
      match args with
      | Wasm.Values.[ Num (I64 x) ] ->
          let x = Vec.get_and_remove vec x in
          let result = Vec.alloc vec (Union (Left x)) in
          wasm_i64 result
      | _ -> raise Type_error )

let right =
  ( to_utf8 "right",
    [ i64 ] -%> [ i64 ],
    fun inst args ->
      Wasm.Instance.burn_gas !inst 100L;

      match args with
      | Wasm.Values.[ Num (I64 x) ] ->
          let x = Vec.get_and_remove vec x in
          let result = Vec.alloc vec (Union (Right x)) in
          wasm_i64 result
      | _ -> raise Type_error )

let closure =
  ( to_utf8 "closure",
    [ i32 ] -%> [ i64 ],
    fun inst args ->
      Wasm.Instance.burn_gas !inst 100L;

      match args with
      | Wasm.Values.[ Num (I32 x) ] ->
          let result = Vec.alloc vec (Closure { opt_arg = None; call = x }) in
          wasm_i64 result
      | _ -> raise Type_error )

let cdr =
  ( to_utf8 "cdr",
    [ i64 ] -%> [ i64 ],
    fun inst args ->
      Wasm.Instance.burn_gas !inst 100L;
      match args with
      | Wasm.Values.[ Num (I64 x) ] ->
          let _, x = Vec.get_and_remove vec x %-< pair in
          let result = Vec.alloc vec x in
          wasm_i64 result
      | _ -> raise Type_error )

let compare_v =
  let typ = [ i64; i64 ] -%> [ i64 ] in
  ( to_utf8 "compare",
    typ,
    fun inst args ->
      Wasm.Instance.burn_gas !inst 100L;
      match args with
      | Wasm.Values.[ Num (I64 x); Num (I64 y) ] ->
          let x = Vec.get_and_remove vec x in
          let y = Vec.get_and_remove vec y in
          let result = Int (Z.of_int (Value.compare x y)) in
          let result = Vec.alloc vec result in
          wasm_i64 result
      | _ -> raise Type_error )

(* let equal =
   let typ = [ i64; i64 ] -%> [ i64 ] in
   ( to_utf8 "equal",
     typ,
     fun inst args ->
       Wasm.Instance.burn_gas !inst 100L;
       match args with
       | Wasm.Values.[ Num (I64 x); Num (I64 y) ] ->
           let x = Vec.get_and_remove vec x in
           let y = Vec.get_and_remove vec y in
           let result = Value.equal x y in
           wasm_i64 (Int64.of_int @@ Bool.to_int result)
       | _ -> raise Type_error ) *)

let mem =
  let typ = [ i64; i64 ] -%> [ i64 ] in
  let matcher = function
    | x, Map m -> Map.mem x m
    | x, Set s -> Set.mem x s
    | _ -> raise Type_error
  in
  ( to_utf8 "mem",
    typ,
    fun inst args ->
      Wasm.Instance.burn_gas !inst 100L;
      match args with
      | Wasm.Values.[ Num (I64 k); Num (I64 data) ] ->
          let x = Vec.get_and_remove vec k in
          let y = Vec.get_and_remove vec data in
          let result = matcher (x, y) in
          wasm_i64 (Int64.of_int @@ Bool.to_int result)
      | _ -> raise Type_error )

let map_get =
  let typ = [ i64; i64 ] -%> [ i64 ] in
  let matcher = function
    | x, Map m -> Map.find_opt x m
    | _ -> raise Type_error
  in
  ( to_utf8 "map_get",
    typ,
    fun inst args ->
      Wasm.Instance.burn_gas !inst 100L;
      match args with
      | Wasm.Values.[ Num (I64 k); Num (I64 data) ] ->
          let x = Vec.get_and_remove vec k in
          let y = Vec.get_and_remove vec data in
          let result = matcher (x, y) in
          wasm_i64 (Vec.alloc vec (Option result))
      | _ -> raise Type_error )

let update_ =
  let typ = [ i64; i64; i64 ] -%> [ i64 ] in
  let matcher = function
    | x, Option (Some v), Map m -> Map (Map.add x v m)
    | x, Option None, Map m -> Map (Map.remove x m)
    | x, Bool v, Set m -> Set (if v = 1 then Set.add x m else Set.remove x m)
    | _ -> raise Type_error
  in
  ( to_utf8 "update",
    typ,
    fun inst args ->
      Wasm.Instance.burn_gas !inst 100L;
      match args with
      | Wasm.Values.[ Num (I64 k); Num (I64 value); Num (I64 data) ] ->
          let x = Vec.get_and_remove vec k in
          let zx = Vec.get_and_remove vec value in
          let y = Vec.get_and_remove vec data in
          let result = matcher (x, zx, y) in
          wasm_i64 (Vec.alloc vec result)
      | _ -> raise Type_error )

let transfer_tokens =
  let typ = [ i64; i64; i64 ] -%> [ i64 ] in
  let matcher = function
    | (String _ as x), any -> Pair (x, any)
    | _ -> raise Type_error
  in
  ( to_utf8 "transfer_tokens",
    typ,
    fun inst args ->
      Wasm.Instance.burn_gas !inst 100L;
      match args with
      | Wasm.Values.[ Num (I64 k); Num (I64 value); Num (I64 data) ] ->
          let x = Vec.get_and_remove vec k in
          let _ = Vec.get_and_remove vec value in
          let y = Vec.get_and_remove vec data in
          let result = matcher (x, y) in
          wasm_i64 (Vec.alloc vec result)
      | _ -> raise Type_error )

let ticket =
  let typ = [ i64; i64 ] -%> [ i64 ] in
  let matcher = function
    | Bytes x, Int amount when Z.geq amount Z.zero ->
        Ticket_handle
          (mint_ticket
             (Deku_ledger.Ticket_id.make (Deku (self_addr ())) x)
             (Deku_stdlib.N.of_z amount |> Option.get))
    | _ -> raise Type_error
  in
  ( to_utf8 "ticket",
    typ,
    fun inst args ->
      Wasm.Instance.burn_gas !inst 100L;
      match args with
      | Wasm.Values.[ Num (I64 k); Num (I64 data) ] ->
          let x = Vec.get_and_remove vec k in
          let y = Vec.get_and_remove vec data in
          let result = matcher (x, y) in
          wasm_i64 (Vec.alloc vec result)
      | _ -> raise Type_error )

let split_ticket =
  let typ = [ i64; i64 ] -%> [ i64 ] in
  let matcher = function
    | Ticket_handle handle, Int x, Int y when Z.geq x Z.zero && Z.geq y Z.zero
      -> (
        match
          split_ticket handle
            ( Deku_stdlib.N.of_z x |> Option.get,
              Deku_stdlib.N.of_z y |> Option.get )
        with
        | None -> Option None
        | Some (x, y) -> Option (Some (Pair (Ticket_handle x, Ticket_handle y)))
        )
    | _ -> raise Type_error
  in
  ( to_utf8 "split_ticket",
    typ,
    fun inst args ->
      Wasm.Instance.burn_gas !inst 100L;
      match args with
      | Wasm.Values.[ Num (I64 k); Num (I64 data) ] ->
          let x = Vec.get_and_remove vec k in
          let amount1, amount2 = Vec.get_and_remove vec data %-< pair in
          let result = matcher (x, amount1, amount2) in
          wasm_i64 (Vec.alloc vec result)
      | _ -> raise Type_error )

let join_tickets =
  let typ = [ i64 ] -%> [ i64 ] in
  let matcher = function
    | Pair (Ticket_handle x, Ticket_handle y) -> (
        match join_tickets x y with
        | None -> Option None
        | Some x -> Option (Some (Ticket_handle x)))
    | _ -> raise Type_error
  in
  ( to_utf8 "join_tickets",
    typ,
    fun inst args ->
      Wasm.Instance.burn_gas !inst 100L;
      match args with
      | Wasm.Values.[ Num (I64 k) ] ->
          let x = Vec.get_and_remove vec k in
          let result = matcher x in
          wasm_i64 (Vec.alloc vec result)
      | _ -> raise Type_error )

let read_ticket =
  let typ = [ i64 ] -%> [] in
  let matcher = function
    | Ticket_handle x ->
        let Ticket_id id, amount, handle = read_ticket x in
        let ticketer =
          Deku_ledger.(
            match id.ticketer with
            | Deku x -> Contract_address.to_b58 x
            | Tezos x -> Deku_tezos.Contract_hash.to_b58 x)
        in
        let amount = Deku_stdlib.N.to_z amount in
        push_to_stack
        @@ wasm_i64'
             (Vec.alloc vec
             @@ Pair (String ticketer, Pair (Bytes id.data, Int amount)));
        push_to_stack @@ wasm_i64' (Vec.alloc vec @@ Ticket_handle handle)
    | _ -> raise Type_error
  in
  ( to_utf8 "read_ticket",
    typ,
    fun inst args ->
      Wasm.Instance.burn_gas !inst 100L;
      match args with
      | Wasm.Values.[ Num (I64 k) ] ->
          let x = Vec.get_and_remove vec k in
          matcher x;
          []
      | _ -> raise Type_error )

let get_and_update_ =
  let typ = [ i64; i64; i64 ] -%> [] in
  let matcher = function
    | x, Option (Some v), Map m ->
        let prev_binding = Map.find_opt x m in
        push_to_stack @@ wasm_i64' @@ Vec.alloc vec (Map (Map.add x v m));
        push_to_stack @@ wasm_i64' @@ Vec.alloc vec (Option prev_binding)
    | x, Option None, Map m ->
        let prev_binding = Map.find_opt x m in
        push_to_stack @@ wasm_i64' @@ Vec.alloc vec (Map (Map.remove x m));
        push_to_stack @@ wasm_i64' @@ Vec.alloc vec (Option prev_binding)
    | _ -> raise Type_error
  in
  ( to_utf8 "get_and_update",
    typ,
    fun inst args ->
      Wasm.Instance.burn_gas !inst 100L;
      match args with
      | Wasm.Values.[ Num (I64 k); Num (I64 value); Num (I64 data) ] ->
          let x = Vec.get_and_remove vec k in
          let zx = Vec.get_and_remove vec value in
          let y = Vec.get_and_remove vec data in
          matcher (x, zx, y);
          []
      | _ -> raise Type_error )

let neq =
  let typ = [ i64 ] -%> [ i64 ] in
  ( to_utf8 "neq",
    typ,
    fun inst args ->
      Wasm.Instance.burn_gas !inst 100L;
      match args with
      | Wasm.Values.[ Num (I64 x) ] ->
          let x = Vec.get_and_remove vec x %-< int in
          let result = if Z.(not @@ equal zero x) then Bool 1 else Bool 0 in
          let result = Vec.alloc vec result in
          wasm_i64 result
      | _ -> raise Type_error )

let eq =
  let typ = [ i64 ] -%> [ i64 ] in
  ( to_utf8 "eq",
    typ,
    fun inst args ->
      Wasm.Instance.burn_gas !inst 100L;
      match args with
      | Wasm.Values.[ Num (I64 x) ] ->
          let x = Vec.get_and_remove vec x %-< int in
          let result = if Z.(equal zero x) then Bool 1 else Bool 0 in
          let result = Vec.alloc vec result in
          wasm_i64 result
      | _ -> raise Type_error )

let gt =
  let typ = [ i64 ] -%> [ i64 ] in
  ( to_utf8 "gt",
    typ,
    fun inst args ->
      Wasm.Instance.burn_gas !inst 100L;
      match args with
      | Wasm.Values.[ Num (I64 x) ] ->
          let x = Vec.get_and_remove vec x %-< int in
          let result = if Z.(gt x zero) then Bool 1 else Bool 0 in
          let result = Vec.alloc vec result in
          wasm_i64 result
      | _ -> raise Type_error )

let ge =
  let typ = [ i64 ] -%> [ i64 ] in
  ( to_utf8 "ge",
    typ,
    fun inst args ->
      Wasm.Instance.burn_gas !inst 100L;
      match args with
      | Wasm.Values.[ Num (I64 x) ] ->
          let x = Vec.get_and_remove vec x %-< int in
          let result = if Z.(geq x zero) then Bool 1 else Bool 0 in
          let result = Vec.alloc vec result in
          wasm_i64 result
      | _ -> raise Type_error )

let lt =
  let typ = [ i64 ] -%> [ i64 ] in
  ( to_utf8 "lt",
    typ,
    fun inst args ->
      Wasm.Instance.burn_gas !inst 100L;
      match args with
      | Wasm.Values.[ Num (I64 x) ] ->
          let x = Vec.get_and_remove vec x %-< int in
          let result = if Z.(lt x zero) then Bool 1 else Bool 0 in
          let result = Vec.alloc vec result in
          wasm_i64 result
      | _ -> raise Type_error )

let le =
  let typ = [ i64 ] -%> [ i64 ] in
  ( to_utf8 "le",
    typ,
    fun inst args ->
      Wasm.Instance.burn_gas !inst 100L;
      match args with
      | Wasm.Values.[ Num (I64 x) ] ->
          let x = Vec.get_and_remove vec x %-< int in
          let result = if Z.(leq x zero) then Bool 1 else Bool 1 in
          let result = Vec.alloc vec result in
          wasm_i64 result
      | _ -> raise Type_error )

let or_ =
  let or_ = function
    | Bool x, Bool y -> Bool (x lor y)
    | Int x, Int y -> Int Z.(x lor y)
    | _ -> raise Type_error
  in
  let typ = [ i64; i64 ] -%> [ i64 ] in
  ( to_utf8 "or",
    typ,
    fun inst args ->
      Wasm.Instance.burn_gas !inst 100L;
      match args with
      | Wasm.Values.[ Num (I64 x); Num (I64 y) ] ->
          let x = Vec.get_and_remove vec x in
          let y = Vec.get_and_remove vec y in
          let result = or_ (x, y) in
          let result = Vec.alloc vec result in
          wasm_i64 result
      | _ -> raise Type_error )

let iter_ =
  let matcher idx = function
    | List (x, _) ->
        List.iter
          (fun x -> call_indirect_unit idx (wasm_i64 @@ Vec.alloc vec x))
          x
    | Map x ->
        Map.iter
          (fun k v ->
            call_indirect_unit idx (wasm_i64 @@ Vec.alloc vec (Pair (k, v))))
          x
    | Set x ->
        Set.iter
          (fun x -> call_indirect_unit idx (wasm_i64 @@ Vec.alloc vec x))
          x
    | _ -> raise Type_error
  in
  let typ = [ i64; i32 ] -%> [] in
  ( to_utf8 "iter",
    typ,
    fun inst args ->
      Wasm.Instance.burn_gas !inst 100L;
      match args with
      | Wasm.Values.[ Num (I64 x); Num (I32 y) ] ->
          let x = Vec.get_and_remove vec x in
          matcher y x;
          []
      | _ -> raise Type_error )

let map_ =
  let matcher idx = function
    | List (x, _) -> (
        let res =
          List.map
            (fun x ->
              call_indirect idx (wasm_i64 @@ Vec.alloc vec x)
              |> Vec.get_and_remove vec)
            x
        in
        match res with
        | Bytes _ :: _ -> List (res, Bytes)
        | _ -> List (res, Other))
    | Map x ->
        Map
          (Map.to_seq x
          |> Seq.map (fun (k, v) ->
                 let result =
                   call_indirect idx (wasm_i64 @@ Vec.alloc vec (Pair (k, v)))
                   |> Vec.get_and_remove vec
                 in
                 match result with
                 | Pair (k, v) -> (k, v)
                 | _ -> raise Type_error)
          |> Map.of_seq)
    | Set x ->
        Set
          (Set.map
             (fun x ->
               call_indirect idx (wasm_i64 @@ Vec.alloc vec x)
               |> Vec.get_and_remove vec)
             x)
    | _ -> raise Type_error
  in
  let typ = [ i64; i32 ] -%> [ i64 ] in
  ( to_utf8 "map",
    typ,
    fun inst args ->
      Wasm.Instance.burn_gas !inst 100L;
      match args with
      | Wasm.Values.[ Num (I64 x); Num (I32 y) ] ->
          let x = Vec.get_and_remove vec x in
          wasm_i64 @@ Vec.alloc vec @@ matcher y x
      | _ -> raise Type_error )

let xor_ =
  let xor_ = function
    | Bool x, Bool y -> Bool (x lxor y)
    | Int x, Int y -> Int Z.(x lxor y)
    | _ -> raise Type_error
  in
  let typ = [ i64; i64 ] -%> [ i64 ] in
  ( to_utf8 "xor",
    typ,
    fun inst args ->
      Wasm.Instance.burn_gas !inst 100L;
      match args with
      | Wasm.Values.[ Num (I64 x); Num (I64 y) ] ->
          let x = Vec.get_and_remove vec x in
          let y = Vec.get_and_remove vec y in
          let result = xor_ (x, y) in
          let result = Vec.alloc vec result in
          wasm_i64 result
      | _ -> raise Type_error )

let apply =
  let matcher v = function
    | Closure { opt_arg; call } -> (
        match opt_arg with
        | None ->
            wasm_i64 @@ Vec.alloc vec @@ Closure { opt_arg = Some v; call }
        | Some x ->
            wasm_i64 @@ Vec.alloc vec
            @@ Closure { opt_arg = Some (Pair (v, x)); call })
    | _ -> raise Type_error
  in
  let typ = [ i64; i64 ] -%> [ i64 ] in
  ( to_utf8 "apply",
    typ,
    fun inst args ->
      Wasm.Instance.burn_gas !inst 100L;
      match args with
      | Wasm.Values.[ Num (I64 x); Num (I64 y) ] ->
          let x = Vec.get_and_remove vec x in
          let y = Vec.get_and_remove vec y in
          matcher y x
      | _ -> raise Type_error )

let exec =
  let matcher v = function
    | Closure { opt_arg; call } -> (
        match opt_arg with
        | None -> call_indirect call (wasm_i64 @@ Vec.alloc vec v)
        | Some x -> call_indirect call (wasm_i64 @@ Vec.alloc vec (Pair (v, x)))
        )
    | _ -> raise Type_error
  in
  let typ = [ i64; i64 ] -%> [ i64 ] in
  ( to_utf8 "exec",
    typ,
    fun inst args ->
      Wasm.Instance.burn_gas !inst 100L;
      match args with
      | Wasm.Values.[ Num (I64 x); Num (I64 y) ] ->
          let x = Vec.get_and_remove vec x in
          let y = Vec.get_and_remove vec y in
          wasm_i64 @@ matcher y x
      | _ -> raise Type_error )

let and_ =
  let and_ = function
    | Bool x, Bool y -> Bool (x land y)
    | Int x, Int y -> Int Z.(x land y)
    | _ -> raise Type_error
  in
  let typ = [ i64; i64 ] -%> [ i64 ] in
  ( to_utf8 "and",
    typ,
    fun inst args ->
      Wasm.Instance.burn_gas !inst 100L;
      match args with
      | Wasm.Values.[ Num (I64 x); Num (I64 y) ] ->
          let x = Vec.get_and_remove vec x in
          let y = Vec.get_and_remove vec y in
          let result = and_ (x, y) in
          let result = Vec.alloc vec result in
          wasm_i64 result
      | _ -> raise Type_error )

let not =
  let typ = [ i64 ] -%> [ i64 ] in
  ( to_utf8 "not",
    typ,
    fun inst args ->
      Wasm.Instance.burn_gas !inst 100L;
      match args with
      | Wasm.Values.[ Num (I64 x) ] ->
          let x = Vec.get_and_remove vec x %-< bool in
          let result = Bool (if Int.equal 0 x then 1 else 0) in
          let result = Vec.alloc vec result in
          wasm_i64 result
      | _ -> raise Type_error )

let pair_ =
  let typ = [ i64; i64 ] -%> [ i64 ] in
  ( to_utf8 "pair",
    typ,
    fun inst args ->
      Wasm.Instance.burn_gas !inst 100L;
      match args with
      | Wasm.Values.[ Num (I64 x); Num (I64 y) ] ->
          let x = Vec.get_and_remove vec x in
          let y = Vec.get_and_remove vec y in
          let result = Pair (x, y) in
          let result = Vec.alloc vec result in
          wasm_i64 result
      | _ -> raise Type_error )

let unpair_ =
  let typ = [ i64 ] -%> [] in
  ( to_utf8 "unpair",
    typ,
    fun inst args ->
      Wasm.Instance.burn_gas !inst 100L;
      match args with
      | Wasm.Values.[ Num (I64 x) ] ->
          let fst, snd = Vec.get_and_remove vec x %-< pair in
          let fst = Vec.alloc vec fst in
          let snd = Vec.alloc vec snd in
          push_to_stack @@ wasm_i64' snd;
          push_to_stack @@ wasm_i64' fst;
          []
      | _ -> raise Type_error )

let z_add =
  let typ = [ i64; i64 ] -%> [ i64 ] in
  ( to_utf8 "z_add",
    typ,
    fun inst args ->
      Wasm.Instance.burn_gas !inst 100L;
      match args with
      | Wasm.Values.[ Num (I64 x); Num (I64 y) ] ->
          let x = Vec.get_and_remove vec x %-< int in
          let y = Vec.get_and_remove vec y %-< int in
          let result = Int (Z.add x y) in
          let result = Vec.alloc vec result in
          wasm_i64 result
      | _ -> raise Type_error )

let z_sub =
  let typ = [ i64; i64 ] -%> [ i64 ] in
  ( to_utf8 "z_sub",
    typ,
    fun inst args ->
      Wasm.Instance.burn_gas !inst 100L;
      match args with
      | Wasm.Values.[ Num (I64 x); Num (I64 y) ] ->
          let x = Vec.get_and_remove vec x %-< int in
          let y = Vec.get_and_remove vec y %-< int in
          let result = Int (Z.sub x y) in
          let result = Vec.alloc vec result in
          wasm_i64 result
      | _ -> raise Type_error )

let z_mul =
  let typ = [ i64; i64 ] -%> [ i64 ] in
  ( to_utf8 "z_mul",
    typ,
    fun inst args ->
      Wasm.Instance.burn_gas !inst 100L;
      match args with
      | Wasm.Values.[ Num (I64 x); Num (I64 y) ] ->
          let x = Vec.get_and_remove vec x %-< int in
          let y = Vec.get_and_remove vec y %-< int in
          let result = Int (Z.mul x y) in
          let result = Vec.alloc vec result in
          wasm_i64 result
      | _ -> raise Type_error )

let ediv =
  let typ = [ i64; i64 ] -%> [ i64 ] in
  ( to_utf8 "ediv",
    typ,
    fun inst args ->
      Wasm.Instance.burn_gas !inst 100L;
      match args with
      | Wasm.Values.[ Num (I64 x); Num (I64 y) ] ->
          let x = Vec.get_and_remove vec x %-< int in
          let y = Vec.get_and_remove vec y %-< int in
          if Z.equal Z.zero y then
            let result = Option None in
            let result = Vec.alloc vec result in
            wasm_i64 result
          else
            let quot, rem = Z.ediv_rem x y in
            let result = Option (Some (Pair (Int quot, Int rem))) in
            let result = Vec.alloc vec result in
            wasm_i64 result
      | _ -> raise Type_error )

let lsl_ =
  let typ = [ i64; i64 ] -%> [ i64 ] in
  ( to_utf8 "lsl",
    typ,
    fun inst args ->
      Wasm.Instance.burn_gas !inst 100L;
      match args with
      | Wasm.Values.[ Num (I64 x); Num (I64 y) ] ->
          let x = Vec.get_and_remove vec x %-< int in
          let y = Vec.get_and_remove vec y %-< int in
          let y = try Z.to_int y with Z.Overflow -> raise Type_error in
          let result = Int Z.(x lsl y) in
          let result = Vec.alloc vec result in
          wasm_i64 result
      | _ -> raise Type_error )

let cons =
  let typ = [ i64; i64 ] -%> [ i64 ] in
  let matcher v = function
    | List (x, tag) -> List (v :: x, tag)
    | _ -> raise Type_error
  in
  ( to_utf8 "cons",
    typ,
    fun inst args ->
      Wasm.Instance.burn_gas !inst 100L;
      match args with
      | Wasm.Values.[ Num (I64 x); Num (I64 y) ] ->
          let x = Vec.get_and_remove vec x in
          let y = Vec.get_and_remove vec y in
          let result = Vec.alloc vec (matcher x y) in
          wasm_i64 result
      | _ -> raise Type_error )

let get_n =
  let typ = [ i32; i64 ] -%> [ i64 ] in
  ( to_utf8 "get_n",
    typ,
    fun inst args ->
      match args with
      | Wasm.Values.[ Num (I32 x); Num (I64 y) ] ->
          Wasm.Instance.burn_gas !inst Int64.(mul 100L (Int64.of_int32 x));
          if x = 0l then wasm_i64 y
          else
            let rec go idx current =
              match (idx, current) with
              | 1l, Pair (x, _) -> wasm_i64 @@ Vec.alloc vec x
              | 2l, Pair (_, x) -> wasm_i64 @@ Vec.alloc vec x
              | n, Pair (_, x) -> go Int32.(sub n 1l) x
              | _ -> raise Type_error
            in
            go x (Vec.get_and_remove vec y)
      | _ -> raise Type_error )

let lsr_ =
  let typ = [ i64; i64 ] -%> [ i64 ] in
  ( to_utf8 "lsr",
    typ,
    fun inst args ->
      Wasm.Instance.burn_gas !inst 100L;
      match args with
      | Wasm.Values.[ Num (I64 x); Num (I64 y) ] ->
          let x = Vec.get_and_remove vec x %-< int in
          let y = Vec.get_and_remove vec y %-< int in
          let y = try Z.to_int y with Z.Overflow -> raise Type_error in
          let result = Int Z.(x asr y) in
          let result = Vec.alloc vec result in
          wasm_i64 result
      | _ -> raise Type_error )

let concat_ =
  let concat_ = function
    | List (x, Bytes), _ ->
        let buf = Buffer.create 256 in
        List.iter
          (function Bytes x -> Buffer.add_bytes buf x | _ -> raise Type_error)
          x;
        Bytes (Buffer.to_bytes buf)
    | List (x, String), _ ->
        let buf = Buffer.create 256 in
        List.iter
          (function
            | String x -> Buffer.add_string buf x | _ -> raise Type_error)
          x;
        String (Bytes.to_string @@ Buffer.to_bytes buf)
    | Bytes x, Bytes y -> Bytes (Bytes.concat x [ y ])
    | String x, String y -> String (x ^ y)
    | _ -> raise Type_error
  in
  let typ = [ i64; i64 ] -%> [ i64 ] in
  ( to_utf8 "concat",
    typ,
    fun inst args ->
      Wasm.Instance.burn_gas !inst 100L;
      match args with
      | Wasm.Values.[ Num (I64 x); Num (I64 y) ] ->
          let x = Vec.get_and_remove vec x in
          let y =
            match x with List _ -> Unit | _ -> Vec.get_and_remove vec y
          in
          let result = concat_ (x, y) in
          let result = Vec.alloc vec result in
          wasm_i64 result
      | _ -> raise Type_error )

let is_left =
  let typ = [ i64 ] -%> [ i32 ] in
  ( to_utf8 "if_left",
    typ,
    fun inst args ->
      Wasm.Instance.burn_gas !inst 100L;
      match args with
      | Wasm.Values.[ Num (I64 x) ] -> (
          let un = Vec.get_and_remove vec x %-< union in
          match un with
          | Left x ->
              push_to_stack @@ wasm_i64' (Vec.alloc vec x);
              wasm_i32 1l
          | Right x ->
              push_to_stack @@ wasm_i64' (Vec.alloc vec x);
              wasm_i32 0l)
      | _ -> raise Type_error )

let deref_bool =
  let typ = [ i64 ] -%> [ i32 ] in
  ( to_utf8 "deref_bool",
    typ,
    fun inst args ->
      Wasm.Instance.burn_gas !inst 100L;
      match args with
      | Wasm.Values.[ Num (I64 x) ] ->
          let x = Vec.get_and_remove vec x %-< bool in
          wasm_i32 @@ Int32.of_int x
      | _ -> raise Type_error )

let failwith_ =
  let typ = [ i64 ] -%> [] in
  ( to_utf8 "failwith",
    typ,
    fun inst args ->
      Wasm.Instance.burn_gas !inst 100L;
      match args with
      | Wasm.Values.[ Num (I64 x) ] ->
          let x = Vec.get_and_remove vec x %-< string in
          failwith x
      | _ -> raise Type_error )

let if_none =
  let typ = [ i64 ] -%> [ i32 ] in
  ( to_utf8 "if_none",
    typ,
    fun inst args ->
      Wasm.Instance.burn_gas !inst 100L;
      match args with
      | Wasm.Values.[ Num (I64 x) ] -> (
          let x = Vec.get_and_remove vec x %-< option in
          match x with
          | Some x ->
              push_to_stack @@ wasm_i64' (Vec.alloc vec x);
              wasm_i32 0l
          | None -> wasm_i32 1l)
      | _ -> raise Type_error )

let neg_ =
  let typ = [ i64 ] -%> [ i64 ] in
  ( to_utf8 "neg",
    typ,
    fun inst args ->
      Wasm.Instance.burn_gas !inst 100L;
      match args with
      | Wasm.Values.[ Num (I64 x) ] ->
          let x = Vec.get_and_remove vec x %-< int in
          wasm_i64 (Vec.alloc vec (Int (Z.neg x)))
      | _ -> raise Type_error )

let abs_ =
  let typ = [ i64 ] -%> [ i64 ] in
  ( to_utf8 "abs",
    typ,
    fun inst args ->
      Wasm.Instance.burn_gas !inst 100L;
      match args with
      | Wasm.Values.[ Num (I64 x) ] ->
          let x = Vec.get_and_remove vec x %-< int in
          wasm_i64 (Vec.alloc vec (Int (Z.abs x)))
      | _ -> raise Type_error )

let is_nat =
  let typ = [ i64 ] -%> [ i64 ] in
  ( to_utf8 "isnat",
    typ,
    fun inst args ->
      Wasm.Instance.burn_gas !inst 100L;
      match args with
      | Wasm.Values.[ Num (I64 x) ] ->
          let x = Vec.get_and_remove vec x %-< int in
          let result = Option (if Z.geq x Z.zero then Some (Int x) else None) in
          wasm_i64 (Vec.alloc vec result)
      | _ -> raise Type_error )

let to_int =
  let typ = [ i64 ] -%> [ i64 ] in
  ( to_utf8 "int",
    typ,
    fun inst args ->
      Wasm.Instance.burn_gas !inst 100L;
      match args with
      | Wasm.Values.[ Num (I64 x) ] ->
          let result = Int (Z.of_int64 x) in
          wasm_i64 (Vec.alloc vec result)
      | _ -> raise Type_error )

let address =
  let typ = [ i64 ] -%> [ i64 ] in
  ( to_utf8 "address",
    typ,
    fun inst args ->
      Wasm.Instance.burn_gas !inst 100L;
      match args with
      | Wasm.Values.[ Num (I64 x) ] -> wasm_i64 x
      | _ -> raise Type_error )

let implicit_account =
  let typ = [ i64 ] -%> [ i64 ] in
  ( to_utf8 "implicit_account",
    typ,
    fun inst args ->
      Wasm.Instance.burn_gas !inst 100L;
      match args with
      | Wasm.Values.[ Num (I64 x) ] -> wasm_i64 x
      | _ -> raise Type_error )

let contract =
  let typ = [ i64 ] -%> [ i64 ] in
  ( to_utf8 "contract",
    typ,
    fun inst args ->
      Wasm.Instance.burn_gas !inst 100L;
      match args with
      | Wasm.Values.[ Num (I64 x) ] ->
          let x = Vec.get_and_remove vec x %-< string in
          wasm_i64 (Vec.alloc vec (Option (Some (String x))))
      | _ -> raise Type_error )

let size_ =
  let typ = [ i64 ] -%> [ i64 ] in
  let matcher = function
    | String s -> String.length s
    | Bytes s -> Bytes.length s
    | List (x, _) -> List.length x
    | Map m -> Map.cardinal m
    | Set s -> Set.cardinal s
    | _ -> raise Type_error
  in
  ( to_utf8 "size",
    typ,
    fun inst args ->
      Wasm.Instance.burn_gas !inst 100L;
      match args with
      | Wasm.Values.[ Num (I64 x) ] ->
          let x = Vec.get_and_remove vec x in
          wasm_i64 (Vec.alloc vec (Int (Z.of_int @@ matcher x)))
      | _ -> raise Type_error )

let unpack_ =
  let typ = [ i64 ] -%> [ i64 ] in
  let matcher = function
    | Bytes s -> (
        match Data_encoding.Binary.of_bytes Value.encoding s with
        | Ok x -> x
        | Error _ -> raise Type_error)
    | _ -> raise Type_error
  in
  ( to_utf8 "unpack",
    typ,
    fun inst args ->
      Wasm.Instance.burn_gas !inst 100L;
      match args with
      | Wasm.Values.[ Num (I64 x) ] ->
          let x = Vec.get_and_remove vec x in
          wasm_i64 (Vec.alloc vec (matcher x))
      | _ -> raise Type_error )

let blake2b =
  let typ = [ i64 ] -%> [ i64 ] in
  let matcher = function
    | Bytes s ->
        Bytes
          (String.to_bytes @@ Deku_crypto.BLAKE2b.BLAKE2b_160.to_hex
          @@ Deku_crypto.BLAKE2b.BLAKE2b_160.hash (Bytes.to_string s))
    | _ -> raise Type_error
  in
  ( to_utf8 "blake2b",
    typ,
    fun inst args ->
      Wasm.Instance.burn_gas !inst 100L;
      match args with
      | Wasm.Values.[ Num (I64 x) ] ->
          let x = Vec.get_and_remove vec x in
          wasm_i64 (Vec.alloc vec (matcher x))
      | _ -> raise Type_error )

let keccak3 =
  let typ = [ i64 ] -%> [ i64 ] in
  let matcher = function
    | Bytes s ->
        Bytes
          (String.to_bytes
          @@ Digestif.(KECCAK_256.to_hex @@ KECCAK_256.digest_bytes s))
    | _ -> raise Type_error
  in
  ( to_utf8 "keccak",
    typ,
    fun inst args ->
      Wasm.Instance.burn_gas !inst 100L;
      match args with
      | Wasm.Values.[ Num (I64 x) ] ->
          let x = Vec.get_and_remove vec x in
          wasm_i64 (Vec.alloc vec (matcher x))
      | _ -> raise Type_error )

let sha256 =
  let typ = [ i64 ] -%> [ i64 ] in
  let matcher = function
    | Bytes s ->
        Bytes
          (String.to_bytes @@ Digestif.(SHA256.to_hex @@ SHA256.digest_bytes s))
    | _ -> raise Type_error
  in
  ( to_utf8 "sha256",
    typ,
    fun inst args ->
      Wasm.Instance.burn_gas !inst 100L;
      match args with
      | Wasm.Values.[ Num (I64 x) ] ->
          let x = Vec.get_and_remove vec x in
          wasm_i64 (Vec.alloc vec (matcher x))
      | _ -> raise Type_error )

let sha512 =
  let typ = [ i64 ] -%> [ i64 ] in
  let matcher = function
    | Bytes s ->
        Bytes
          (String.to_bytes @@ Digestif.(SHA512.to_hex @@ SHA512.digest_bytes s))
    | _ -> raise Type_error
  in
  ( to_utf8 "sha512",
    typ,
    fun inst args ->
      Wasm.Instance.burn_gas !inst 100L;
      match args with
      | Wasm.Values.[ Num (I64 x) ] ->
          let x = Vec.get_and_remove vec x in
          wasm_i64 (Vec.alloc vec (matcher x))
      | _ -> raise Type_error )

let sha3 =
  let typ = [ i64 ] -%> [ i64 ] in
  let matcher = function
    | Bytes s ->
        Bytes
          (String.to_bytes
          @@ Digestif.(SHA3_256.to_hex @@ SHA3_256.digest_bytes s))
    | _ -> raise Type_error
  in
  ( to_utf8 "sha3",
    typ,
    fun inst args ->
      Wasm.Instance.burn_gas !inst 100L;
      match args with
      | Wasm.Values.[ Num (I64 x) ] ->
          let x = Vec.get_and_remove vec x in
          wasm_i64 (Vec.alloc vec (matcher x))
      | _ -> raise Type_error )

let pack =
  let typ = [ i64 ] -%> [ i64 ] in
  ( to_utf8 "pack",
    typ,
    fun inst args ->
      Wasm.Instance.burn_gas !inst 100L;
      match args with
      | Wasm.Values.[ Num (I64 x) ] ->
          let x = Vec.get_and_remove vec x in
          wasm_i64
            (Vec.alloc vec
               (Bytes
                  (String.to_bytes
                  @@ Data_encoding.Binary.to_string_exn Value.encoding x)))
      | _ -> raise Type_error )

let dup_host =
  let typ = [ i64 ] -%> [] in
  ( to_utf8 "dup_host",
    typ,
    fun inst args ->
      Wasm.Instance.burn_gas !inst 100L;
      match args with
      | Wasm.Values.[ Num (I64 x) ] ->
          push_to_stack @@ wasm_i64' (Vec.dup vec x);
          []
      | _ -> raise Type_error )

let none_ =
  let typ = [] -%> [ i64 ] in
  ( to_utf8 "none",
    typ,
    fun inst args ->
      Wasm.Instance.burn_gas !inst 100L;
      match (args : Wasm.Values.value list) with
      | [] -> wasm_i64 (Vec.alloc vec (Option None))
      | _ -> raise Type_error )

let nil =
  let typ = [] -%> [ i64 ] in
  ( to_utf8 "nil",
    typ,
    fun inst args ->
      Wasm.Instance.burn_gas !inst 100L;
      match (args : Wasm.Values.value list) with
      | [] -> wasm_i64 (Vec.alloc vec (List ([], Other)))
      | _ -> raise Type_error )

let true_ =
  let typ = [] -%> [ i64 ] in
  ( to_utf8 "true",
    typ,
    fun inst args ->
      Wasm.Instance.burn_gas !inst 100L;
      match (args : Wasm.Values.value list) with
      | [] -> wasm_i64 (Vec.alloc vec (Bool 1))
      | _ -> raise Type_error )

let false_ =
  let typ = [] -%> [ i64 ] in
  ( to_utf8 "false",
    typ,
    fun inst args ->
      Wasm.Instance.burn_gas !inst 100L;
      match (args : Wasm.Values.value list) with
      | [] -> wasm_i64 (Vec.alloc vec (Bool 0))
      | _ -> raise Type_error )

let unit_ =
  let typ = [] -%> [ i64 ] in
  ( to_utf8 "unit",
    typ,
    fun inst args ->
      Wasm.Instance.burn_gas !inst 100L;
      match (args : Wasm.Values.value list) with
      | [] -> wasm_i64 (Vec.alloc vec Unit)
      | _ -> raise Type_error )

let empty_map =
  let typ = [] -%> [ i64 ] in
  ( to_utf8 "empty_map",
    typ,
    fun inst args ->
      Wasm.Instance.burn_gas !inst 100L;
      match (args : Wasm.Values.value list) with
      | [] -> wasm_i64 (Vec.alloc vec (Map Map.empty))
      | _ -> raise Type_error )

let empty_big_map =
  let typ = [] -%> [ i64 ] in
  ( to_utf8 "empty_big_map",
    typ,
    fun inst args ->
      Wasm.Instance.burn_gas !inst 100L;
      match (args : Wasm.Values.value list) with
      | [] -> wasm_i64 (Vec.alloc vec (Map Map.empty))
      | _ -> raise Type_error )

let empty_set =
  let typ = [] -%> [ i64 ] in
  ( to_utf8 "empty_set",
    typ,
    fun inst args ->
      Wasm.Instance.burn_gas !inst 100L;
      match (args : Wasm.Values.value list) with
      | [] -> wasm_i64 (Vec.alloc vec (Set Set.empty))
      | _ -> raise Type_error )

let zero =
  let typ = [] -%> [ i64 ] in
  ( to_utf8 "zero",
    typ,
    fun inst args ->
      Wasm.Instance.burn_gas !inst 100L;
      match (args : Wasm.Values.value list) with
      | [] -> wasm_i64 (Vec.alloc vec (Int Z.zero))
      | _ -> raise Type_error )

let balance =
  let typ = [] -%> [ i64 ] in
  ( to_utf8 "balance",
    typ,
    fun inst args ->
      Wasm.Instance.burn_gas !inst 100L;
      match (args : Wasm.Values.value list) with
      | [] -> wasm_i64 (Vec.alloc vec (Int Z.zero))
      | _ -> raise Type_error )

let self =
  let typ = [] -%> [ i64 ] in
  ( to_utf8 "self",
    typ,
    fun inst args ->
      Wasm.Instance.burn_gas !inst 100L;
      match (args : Wasm.Values.value list) with
      | [] ->
          wasm_i64
            (Vec.alloc vec
               (String (Deku_ledger.Contract_address.to_b58 @@ self_addr ())))
      | _ -> raise Type_error )

let self_address =
  let typ = [] -%> [ i64 ] in
  ( to_utf8 "self_address",
    typ,
    fun inst args ->
      Wasm.Instance.burn_gas !inst 100L;
      match (args : Wasm.Values.value list) with
      | [] ->
          wasm_i64
            (Vec.alloc vec
               (String (Deku_ledger.Contract_address.to_b58 @@ self_addr ())))
      | _ -> raise Type_error )

let sender =
  let typ = [] -%> [ i64 ] in
  ( to_utf8 "sender",
    typ,
    fun inst args ->
      Wasm.Instance.burn_gas !inst 100L;
      match (args : Wasm.Values.value list) with
      | [] ->
          wasm_i64
            (Vec.alloc vec
               (String (Deku_ledger.Address.to_b58 @@ sender_addr ())))
      | _ -> raise Type_error )

let source =
  let typ = [] -%> [ i64 ] in
  ( to_utf8 "source",
    typ,
    fun inst args ->
      Wasm.Instance.burn_gas !inst 100L;
      match (args : Wasm.Values.value list) with
      | [] ->
          wasm_i64
            (Vec.alloc vec
               (String (Deku_ledger.Address.to_b58 @@ source_addr ())))
      | _ -> raise Type_error )

let amount =
  let typ = [] -%> [ i64 ] in
  ( to_utf8 "amount",
    typ,
    fun inst args ->
      Wasm.Instance.burn_gas !inst 100L;
      match (args : Wasm.Values.value list) with
      | [] -> wasm_i64 (Vec.alloc vec @@ Int Z.zero)
      | _ -> raise Type_error )

let const_ =
  let typ = [ i32 ] -%> [ i64 ] in
  ( to_utf8 "const",
    typ,
    fun inst args ->
      Wasm.Instance.burn_gas !inst 100L;
      match args with
      | Wasm.Values.[ Num (I32 x) ] ->
          wasm_i64 (Vec.alloc vec (get_constant (Int64.of_int32 x)))
      | _ -> raise Type_error )

let if_cons =
  let typ = [ i64 ] -%> [ i32 ] in
  let matcher = function
    | List (x :: rest, tag) ->
        push_to_stack @@ wasm_i64' (Vec.alloc vec (List (rest, tag)));
        push_to_stack @@ wasm_i64' (Vec.alloc vec x);
        wasm_i32 1l
    | List ([], _) -> wasm_i32 0l
    | _ -> raise Type_error
  in
  ( to_utf8 "if_cons",
    typ,
    fun inst args ->
      Wasm.Instance.burn_gas !inst 100L;
      match args with
      | Wasm.Values.[ Num (I64 x) ] -> matcher (Vec.get_and_remove vec x)
      | _ -> raise Type_error )

let imports =
  List.map func
    [
      amount;
      const_;
      if_cons;
      empty_big_map;
      source;
      sender;
      self;
      zero;
      const_;
      self_address;
      unit_;
      empty_map;
      false_;
      true_;
      empty_set;
      nil;
      cons;
      none_;
      pack;
      unpack_;
      sha3;
      sha256;
      sha512;
      keccak3;
      blake2b;
      dup_host;
      contract;
      address;
      implicit_account;
      size_;
      abs_;
      is_left;
      is_nat;
      if_none;
      neg_;
      to_int;
      deref_bool;
      failwith_;
      concat_;
      lsr_;
      ediv;
      lsl_;
      get_n;
      z_add;
      z_mul;
      z_sub;
      zero;
      not;
      unpair_;
      pair_;
      apply;
      exec;
      and_;
      iter_;
      map_;
      le;
      or_;
      xor_;
      gt;
      ge;
      neq;
      eq;
      read_ticket;
      get_and_update_;
      split_ticket;
      ticket;
      join_tickets;
      update_;
      transfer_tokens;
      mem;
      map_get;
      compare_v;
      cdr;
      car;
      closure;
      (* equal; *)
      left;
      right;
      balance;
      gt;
      some;
      lt;
    ]

let imports () =
  reset ();
  imports

let alloc (argument, storage) = Vec.alloc vec (Pair (argument, storage))
let read x = Vec.get_and_remove vec x
