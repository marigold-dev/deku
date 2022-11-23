open Core_bench

let incrementer =
  let open Wasm in
  match
    {|
    (module
      (memory 1 100)
      (func (export "addTwo") (param i32 i32) (result i32)
        local.get 0
        local.get 1
        i32.add
        i32.const 55 
        i32.add))
    |}
    |> Parse.string_to_module
  with
  | Source.{ it = Textual m; _ } -> m
  | _ -> assert false

let factorial =
  let open Wasm in
  match
    {| 
  (module
  (memory 1 12)
  (func $fac
    (param $n i32)
    (result i32)
    local.get $n
    i32.const 2
    i32.le_s
    if (result i32)
      local.get $n
    else
      local.get $n
      local.get $n
      i32.const 1
      i32.sub
      call $fac
      i32.mul
    end)
    )
  |}
    |> Parse.string_to_module
  with
  | Source.{ it = Textual m; _ } -> m
  | _ -> assert false

let factorial2 =
  let open Wasm in
  match
    {| 
    (module
    (func $fac
      (param $n i32)
      (result i32)
      local.get $n
      i32.const 2
      i32.le_s
      if (result i32)
        local.get $n
      else
        local.get $n
        local.get $n
        i32.const 1
        i32.sub
        call $fac
        i32.mul
      end)
      (export "fac" (func $fac)))
    |}
    |> Parse.string_to_module
  with
  | Source.{ it = Textual m; _ } -> m
  | _ -> assert false

type _ num = Int32 : int32 num | Int64 : int64 num
type numeric = Numd : 'a num * 'a -> numeric
type by_ref = Ref : 'a ref -> by_ref [@@unboxed]
type binop = Plus | Minus | Mul | Le_s
type value = Num of numeric | Ref' of by_ref

type runtime_state = {
  memory : Wasm.Memory.t;
  locals : value Core.Uniform_array.t;
  functions : return Core.Uniform_array.t;
}

and (_, _) retval =
  | Int : (runtime_state, numeric) retval
  | Ref : (runtime_state, by_ref) retval
  | Value : (runtime_state, value) retval

and return =
  | Return : 'a. (runtime_state, 'a) retval * (runtime_state -> 'a) -> return

type compile_state = { stack : return list } [@@unboxed]

let[@inline always] apply_binaryOp op (Numd (witness, n1) : numeric)
    (Numd (witness2, n2) : numeric) : numeric =
  match (witness, witness2) with
  | Int32, Int32 ->
      let op =
        match op with
        | Plus -> Int32.add n1 n2
        | Minus -> Int32.sub n1 n2
        | Mul -> Int32.mul n1 n2
        | Le_s -> if Core.Int32.( <= ) n1 n2 = true then 1l else 0l
      in
      Numd (Int32, op)
  | Int64, Int64 ->
      let op =
        match op with
        | Plus -> Int64.add n1 n2
        | Minus -> Int64.sub n1 n2
        | Mul -> Int64.mul n1 n2
        | _ -> assert false
      in
      Numd (Int64, op)
  | _ -> failwith "type mismatch"

let[@inline always] get_2 = function
  | x :: y :: rest -> (x, y, rest)
  | _ -> assert false

let[@inline always] get_i32 = function
  | Wasm.Values.Num (I32 x) -> x
  | _ -> failwith "error"

let compile (modu : Wasm.Ast.module_) =
  let open Wasm in
  let open Ast in
  let r = List.hd modu.it.funcs in
  let compiled_locals =
    Core.Uniform_array.of_array
    @@ Array.init 2 (fun _ -> Num (Numd (Int32, 0l)))
  in
  let[@inline always] match_bop b a op =
    match op with
    | Values.I32 x ->
        let op =
          match x with
          | I32Op.Add -> Plus
          | I32Op.Mul -> Mul
          | I32Op.Sub -> Minus
          | _ -> failwith "test"
        in
        apply_binaryOp op b a
    | _ -> assert false
  in
  let[@inline always] match_relop b a op =
    match op with
    | Values.I32 x ->
        let op = match x with I32Op.LeS -> Le_s | _ -> failwith "test" in
        apply_binaryOp op b a
    | _ -> assert false
  in
  let[@inline always] [@warning "-8"] eqq (Numd (Int32, x)) (Numd (Int32, y)) =
    Int32.equal x y
  in
  let[@warning "-8"] rec go state instrs =
    match instrs with
    | Return :: [] -> List.hd state.stack
    | Drop :: xs -> go { stack = List.tl state.stack } xs
    | Binary op :: xs ->
        let Return (witness1, f1), Return (witness2, f2), rest =
          get_2 state.stack
        in
        let stack_top ctx =
          match (witness1, witness2) with
          | Int, Int ->
              let x = f1 ctx in
              let y = f2 ctx in
              match_bop x y op
          | Ref, Ref -> failwith "Todo"
        in
        let rest = go { stack = Return (Int, stack_top) :: rest } xs in
        rest
    | Compare op :: xs ->
        let Return (witness1, f1), Return (witness2, f2), rest =
          get_2 state.stack
        in
        let stack_top ctx =
          match (witness1, witness2) with
          | Int, Int ->
              let x = f1 ctx in
              let y = f2 ctx in
              match_relop x y op
        in
        let rest = go { stack = Return (Int, stack_top) :: rest } xs in
        rest
    | LocalGet id :: xs ->
        let id = Int32.to_int id.Source.it in
        let func (ctx : runtime_state) = Core.Uniform_array.get ctx.locals id in
        let rest =
          go { stack = Return (Value, fun ctx -> func ctx) :: state.stack } xs
        in
        rest
    | LocalSet id :: xs ->
        let hd = List.hd state.stack in
        let stack = List.tl state.stack in
        let rest = go { stack } xs in
        let id = Int32.to_int id.Source.it in
        let res (Return (wt, hd) : return) (Return (rt, rest) : return)
            (ctx : runtime_state) =
          match (wt, rt) with
          | Value, Value ->
              Core.Uniform_array.set_with_caml_modify ctx.locals id (hd ctx)
        in
        Return
          ( Int,
            fun ctx ->
              res hd rest ctx;
              match rest with Return (Int, rest) -> rest ctx )
    | [] -> List.hd state.stack
    | If (_, true_, false_) :: xs ->
        let cond = List.hd state.stack in
        let stack = List.tl state.stack in
        let then_ =
          go { stack = [] } @@ List.map (fun x -> x.Source.it) true_
        in
        let else_ =
          go { stack = [] } @@ List.map (fun x -> x.Source.it) false_
        in
        let conditional (Return (w1, x) : return) ctx =
          match w1 with
          | Int ->
              let endd = x ctx in
              let true_ = Numd (Int32, 1l) in
              if eqq endd true_ then true else false
        in
        go
          {
            stack =
              Return
                ( Int,
                  fun ctx ->
                    if conditional cond ctx then
                      match then_ with Return (Int, x) -> x ctx
                    else match else_ with Return (Int, x) -> x ctx )
              :: stack;
          }
          xs
    | Const { it = I32 x; at = _ } :: xs ->
        go
          { stack = Return (Int, fun ctx -> Numd (Int32, x)) :: state.stack }
          xs
    | Const { it = I64 x; at = _ } :: xs ->
        go
          { stack = Return (Int, fun ctx -> Numd (Int64, x)) :: state.stack }
          xs
    | Call id :: xs ->
        let callback (ctx : runtime_state) =
          let (Return (Value, t)) = List.hd state.stack in
          let loc = Core.Uniform_array.copy ctx.locals in
          let value = t ctx in
          Core.Uniform_array.set_with_caml_modify loc 0 value;
          let ret = Core.Uniform_array.get ctx.functions 0 in
          (ret, { ctx with locals = loc })
        in
        go
          {
            stack =
              Return
                ( Int,
                  fun ctx ->
                    let Return (Int, r), arg = callback ctx in
                    r arg )
              :: List.tl state.stack;
          }
          xs
    | x ->
        List.iter
          (fun x -> Wasm.Print.instr stdout 80 (Source.at Source.no_region x))
          x;
        failwith "test"
  in
  fun memory a ->
    Core.Uniform_array.set_with_caml_modify compiled_locals 0 a;
    let return =
      go { stack = [] } @@ List.map (fun x -> x.Source.it) r.it.body
    in
    ( return,
      {
        memory;
        locals = compiled_locals;
        functions = Core.Uniform_array.of_array [| return |];
      } )

let compile modd =
  (compile modd, Wasm.Memory.alloc (MemoryType { min = 1l; max = Some 12l }))

let run mem fn =
  let r2 = fn mem (Num (Numd (Int32, 8l))) in
  ignore @@ r2

let harness size f =
  let rec go size f =
    if size <= 0 then ()
    else
      let () = ignore (f ()) in
      go (size - 1) f
  in
  go size f

let factorial' n =
  let tr_factorial n =
    let rec aux i accum = if i > n then accum else aux (i + 1) (accum * i) in
    aux 1 1
  in
  tr_factorial n

let initialize m =
  let open Wasm in
  Eval.init m [] ~gas_limit:Int64.max_int

let get_main t =
  match Wasm.Instance.export t (Wasm.Utf8.decode "fac") |> Option.get with
  | Wasm.Instance.ExternFunc x -> x
  | _ -> assert false

let call t =
  let open Wasm in
  match Eval.invoke t [ Wasm.Values.Num (I32 8l) ] with
  | [ Wasm.Values.Num (I32 x) ] -> assert (x = 40320l)
  | _ -> failwith "lifecycle error"

let benchmarks =
  (* should be possible to run once with and keep memory allocated per contract, biggest overhead is the memory zeroing,it will always dominate execution times *)
  let compiled, mem = compile factorial in
  [
    Bench.Test.create_indexed ~name:"WASM SPEC VM, with INIT" ~args:[ 1000 ]
      (fun len ->
        Base.Staged.stage (fun () ->
            ignore
              (harness len (fun () ->
                   let initialized' = factorial2 |> initialize in
                   let initialized = initialized' |> get_main in
                   Wasm.Instance.set_gas_limit initialized' Int64.max_int;
                   call initialized))));
    Bench.Test.create_indexed ~name:"Dynamic dispatch, existentials"
      ~args:[ 1000 ] (fun len ->
        Base.Staged.stage (fun () ->
            ignore (harness len (fun () -> run mem compiled))));
    Bench.Test.create_indexed ~name:"OCAML NATIVE" ~args:[ 1000 ] (fun len ->
        Base.Staged.stage (fun () ->
            ignore (harness len (fun () -> factorial' 8))));
  ]

let () = benchmarks |> Bench.make_command |> Command_unix.run
