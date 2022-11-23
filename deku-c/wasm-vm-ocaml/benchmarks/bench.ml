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

type runtime_state = {
  memory : Wasm.Memory.t;
  locals : Wasm.Values.value ref list;
  functions : (runtime_state -> Wasm.Values.value) Core.Uniform_array.t;
}

type compile_state = { stack : (runtime_state -> Wasm.Values.value) list }
[@@unboxed]

let[@inline always] apply_binaryOp op = Wasm.Values.Num (I32 op)

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
  let[@inline always] match_bop b a op =
    match (op, a, b) with
    | Values.I32 x, Wasm.Values.Num (I32 a), Wasm.Values.Num (I32 b) ->
        let op =
          match x with
          | I32Op.Add -> Int32.add a b
          | I32Op.Mul -> Int32.mul a b
          | I32Op.Sub -> Int32.sub a b
          | _ -> failwith "test"
        in
        apply_binaryOp op
    | _ -> assert false
  in
  let[@inline always] match_relop b a op =
    match (op, a, b) with
    | Values.I32 x, Wasm.Values.Num (I32 a), Wasm.Values.Num (I32 b) ->
        let op =
          match x with
          | I32Op.LeS -> if Core.Int32.(a <= b) then 1l else 0l
          | _ -> failwith "test"
        in
        apply_binaryOp op
    | _ -> assert false
  in

  let[@warning "-8"] rec go state instrs =
    match instrs with
    | Return :: [] -> List.hd state.stack
    | Drop :: xs -> go { stack = List.tl state.stack } xs
    | Binary op :: xs ->
        let x, y, rest = get_2 state.stack in
        let[@inline always] stack_top ctx =
          let x = x ctx in
          let y = y ctx in
          match_bop x y op
        in
        let rest = go { stack = (fun ctx -> stack_top ctx) :: rest } xs in
        rest
    | Compare op :: xs ->
        let x, y, rest = get_2 state.stack in
        let[@inline always] stack_top ctx =
          let x = x ctx in
          let y = y ctx in
          match_relop x y op
        in
        let rest = go { stack = stack_top :: rest } xs in
        rest
    | LocalGet id :: xs ->
        let id = Int32.to_int id.Source.it in
        let[@inline always] func (ctx : runtime_state) =
          !(Core.List.nth_exn ctx.locals id)
        in
        let rest = go { stack = func :: state.stack } xs in
        rest
    | LocalSet id :: xs ->
        let hd = List.hd state.stack in
        let stack = List.tl state.stack in
        let rest = go { stack } xs in
        let id = Int32.to_int id.Source.it in
        fun ctx ->
          Core.List.nth_exn ctx.locals id := hd ctx;
          rest ctx
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
        go
          {
            stack =
              (fun ctx ->
                let (Wasm.Values.Num (I32 endd)) = cond ctx in
                let true_ = 1l in
                if Int32.equal endd true_ then then_ ctx else else_ ctx)
              :: stack;
          }
          xs
    | Const { it = x; at = _ } :: xs ->
        go { stack = (fun ctx -> Num x) :: state.stack } xs
    | Call id :: xs ->
        let[@inline always] callback (ctx : runtime_state) =
          let t = List.hd state.stack in
          let value = t ctx in
          Core.Ref.set_temporarily (Core.List.nth_exn ctx.locals 0) value
            ~f:(fun () ->
              let ret = Core.Uniform_array.get ctx.functions 0 in
              ret ctx)
        in
        go { stack = callback :: List.tl state.stack } xs
    | x ->
        List.iter
          (fun x -> Wasm.Print.instr stdout 80 (Source.at Source.no_region x))
          x;
        failwith "test"
  in
  fun memory a ->
    let return =
      go { stack = [] } @@ List.map (fun x -> x.Source.it) r.it.body
    in
    return
      {
        memory;
        locals = [ ref a ];
        functions = Core.Uniform_array.of_array [| return |];
      }

let compile modd =
  (compile modd, Wasm.Memory.alloc (MemoryType { min = 1l; max = Some 2l }))

let run mem fn =
  let r2 = fn mem (Wasm.Values.Num (I32 8l)) in
  ignore r2

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
  (* should be possible to run once with and keep memory allocated per contract, biggest bottleneck always will be zeroing the memory. *)
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
    Bench.Test.create_indexed ~name:"Dynamic dispatch" ~args:[ 1000 ]
      (fun len ->
        Base.Staged.stage (fun () ->
            ignore (harness len (fun () -> run mem compiled))));
    Bench.Test.create_indexed ~name:"OCAML NATIVE" ~args:[ 1000 ] (fun len ->
        Base.Staged.stage (fun () ->
            ignore (harness len (fun () -> factorial' 8))));
  ]

let () = benchmarks |> Bench.make_command |> Command_unix.run
