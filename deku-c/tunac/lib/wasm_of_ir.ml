open IR
open Binaryen

(* This is not being maintained in favor of llvm_of_ir, do not rely on this. *)

let gensym_count = ref 0
let gensym name =
  incr gensym_count;
  Printf.sprintf "%s.%d" name !gensym_count

let rec compile_expression wasm_mod expr =
  match expr with
  | Cglobal global -> Expression.Global_get.make wasm_mod global Type.int32
  | Cvar var -> Expression.Local_get.make wasm_mod var Type.int32
  | Cconst_i32 value -> Expression.Const.make wasm_mod (Literal.int32 value)
  | Cop (op, params) -> compile_operation wasm_mod op params

and compile_operation wasm_mod op params =
  let compile_load cell typ ptr =
    (* TODO: How know if its signed or not? *)
    let bytes = 
      match typ with
      | I8  | U8  -> 1
      | I32 | U32 -> 4
    in
    Expression.Load.make wasm_mod bytes (cell * 4) 0 Type.int32 ptr
  in

  match op, params with
  | Capply name, params -> Expression.Call.make wasm_mod name (List.map (compile_expression wasm_mod) params) Type.int32
  | Cload (cell, typ), [ ptr ] -> compile_load cell typ (compile_expression wasm_mod ptr)
  | Calloc size, params ->
    let final_size =
      match size, params with
      | 0, [ value ] -> compile_expression wasm_mod value
      | size, [ ] -> Expression.Const.make wasm_mod (Literal.int32 (Int32.of_int (size * 4)))
      | size, [ value ] ->
        Expression.Binary.make wasm_mod Op.add_int32
          (compile_expression wasm_mod value)
          (Expression.Const.make wasm_mod (Literal.int32 (Int32.of_int (size * 4))))
      | _ -> assert false
    in
    Expression.Block.make wasm_mod (gensym "alloc")
      [ Expression.Local_set.make wasm_mod 0 (Expression.Global_get.make wasm_mod "heap_top" Type.int32)
      ; Expression.Global_set.make wasm_mod "heap_top"
          (Expression.Binary.make wasm_mod Op.add_int32
            (Expression.Global_get.make wasm_mod "heap_top" Type.int32)
            final_size)
      ; Expression.Local_get.make wasm_mod 0 Type.int32 ]
  | Cwasm (wasm_operation, typ), params -> compile_wasm_operation wasm_mod typ wasm_operation params

  | _ -> failwith "Invalid operation format, check operation arguments."

and compile_wasm_operation wasm_mod typ operation params =
  let op2 op x y =
    Expression.Binary.make wasm_mod op
      (compile_expression wasm_mod x)
      (compile_expression wasm_mod y)
  in
  let op1 op x =
    Expression.Unary.make wasm_mod op (compile_expression wasm_mod x)
  in
  match operation, typ, params with
  | Wasm_add, _, [ a; b ] -> op2 Op.add_int32 a b
  | Wasm_sub, _, [ a; b ] -> op2 Op.sub_int32 a b
  | Wasm_mul, _, [ a; b ] -> op2 Op.mul_int32 a b
  | Wasm_div, (I32 | I8), [ a; b ] -> op2 Op.div_s_int32 a b
  | Wasm_div, (U32 | U8), [ a; b ] -> op2 Op.div_u_int32 a b
  | Wasm_rem, (I32 | I8), [ a; b ] -> op2 Op.rem_s_int32 a b
  | Wasm_rem, (U32 | U8), [ a; b ] -> op2 Op.rem_u_int32 a b
  | Wasm_and, _, [ a; b ] -> op2 Op.and_int32 a b
  | Wasm_or, _, [ a; b ] -> op2 Op.or_int32 a b
  | Wasm_xor, _, [ a; b ] -> op2 Op.xor_int32 a b
  | Wasm_eq, _, [ a; b ] -> op2 Op.eq_int32 a b
  | Wasm_ne, _, [ a; b ] -> op2 Op.ne_int32 a b
  | Wasm_lt, (I32 | I8), [ a; b ] -> op2 Op.lt_s_int32 a b
  | Wasm_lt, (U32 | U8), [ a; b ] -> op2 Op.lt_u_int32 a b
  | Wasm_gt, (I32 | I8), [ a; b ] -> op2 Op.gt_s_int32 a b
  | Wasm_gt, (U32 | U8), [ a; b ] -> op2 Op.gt_u_int32 a b
  | Wasm_le, (I32 | I8), [ a; b ] -> op2 Op.le_s_int32 a b
  | Wasm_le, (U32 | U8), [ a; b ] -> op2 Op.le_u_int32 a b
  | Wasm_ge, (I32 | I8), [ a; b ] -> op2 Op.ge_s_int32 a b
  | Wasm_ge, (U32 | U8), [ a; b ] -> op2 Op.ge_u_int32 a b
  | Wasm_shl, _, [ a; b ] -> op2 Op.shl_int32 a b
  | Wasm_shr, (I32 | I8), [ a; b ] -> op2 Op.shr_s_int32 a b
  | Wasm_shr, (U32 | U8), [ a; b ] -> op2 Op.shr_u_int32 a b
  | Wasm_rotl, _, [ a; b ] -> op2 Op.rot_l_int32 a b
  | Wasm_rotr, _, [ a; b ] -> op2 Op.rot_r_int32 a b

  | Wasm_clz, _, [ a ] -> op1 Op.clz_int32 a
  | Wasm_ctz, _, [ a ] -> op1 Op.ctz_int32 a
  | Wasm_popcnt, _, [ a ] -> op1 Op.popcnt_int32 a
  | Wasm_eqz, _, [ a ] -> op1 Op.eq_z_int32 a

  | _ -> failwith "Invalid WASM operation"

let loop_stack = ref []

let rec compile_statement wasm_mod statement =
  match statement with
  | Cblock statements ->
    Expression.Block.make wasm_mod (gensym "block")
      (List.map (compile_statement wasm_mod) statements)

  | Cassign (var, expr) ->
      Expression.Local_set.make wasm_mod var (compile_expression wasm_mod expr)

  | Cstore (cell, ptr, value) ->
    Expression.Store.make wasm_mod 4 (cell * 4) 0
      (compile_expression wasm_mod ptr)
      (compile_expression wasm_mod value)
      Type.int32

  | Cglobal_assign (global, value) ->
    Expression.Global_set.make wasm_mod global
      (compile_expression wasm_mod value)

  | Cifthenelse (condition, _if, _else) ->
    Expression.If.make wasm_mod
      (compile_expression wasm_mod condition)
      (compile_statement wasm_mod _if)
      (compile_statement wasm_mod _else)

  | Cwhile (condition, statement) ->
    let name = gensym "loop" in
    loop_stack := name :: !loop_stack;
    let loop =
      Expression.Loop.make wasm_mod name
        (Expression.If.make wasm_mod
          (compile_expression wasm_mod condition)
          (Expression.Block.make wasm_mod (gensym "while_body")
            [ compile_statement wasm_mod statement
            ; Expression.Break.make wasm_mod name
                (Expression.Null.make ())
                (Expression.Null.make ()) ])
          (Expression.Null.make ()))
    in
    loop_stack := List.tl !loop_stack;
    loop

  | Cfailwith param ->
    Expression.Block.make wasm_mod (gensym "failwith")
      [ Expression.Call.make wasm_mod "failwith" [ compile_expression wasm_mod param ] Type.none
      ; Expression.Unreachable.make wasm_mod ]

  | Ccontinue ->
    (* WASM break on loops works more like a continue than a break *)
    Expression.Break.make wasm_mod (List.hd !loop_stack) (Expression.Null.make ()) (Expression.Null.make ())

let add_function wasm_mod name fn =
  let IR_of_michelson.{ body; locals } = fn in
  let locals = Array.make (locals + 1) Type.int32 in
  let expr = compile_statement wasm_mod body in
  ignore @@ Function.add_function wasm_mod name Type.none Type.none locals expr;
  ignore @@ Export.add_function_export wasm_mod name name

let compile_exec_function wasm_mod lambdas =
  let rec aux lambdas =
    match lambdas with
    | (idx, _) :: lambdas ->
      Expression.If.make wasm_mod
        (Expression.Binary.make wasm_mod Op.eq_int32
          (Expression.Local_get.make wasm_mod 0 Type.int32)
          (Expression.Const.make wasm_mod (Literal.int32 (Int32.of_int idx))))
        (Expression.Call.make wasm_mod (Printf.sprintf "lambda_%d" idx) [] Type.none)
        (aux lambdas)
    | [] -> Expression.Unreachable.make wasm_mod
  in
  let body = aux lambdas in
  ignore @@
    Function.add_function wasm_mod "exec"
      Type.(create [| int32 |])
      Type.int32
      [||]
      (Expression.Block.make wasm_mod "exec_func_body" [ body; Expression.Const.make wasm_mod (Literal.int32 0l) ])

let compile_malloc wasm_mod =
  let body =
    Expression.Block.make wasm_mod "malloc_func_body"
      [ Expression.Global_set.make wasm_mod "heap_top"
          (Expression.Binary.make wasm_mod Op.add_int32
            (Expression.Local_tee.make wasm_mod 1 (Expression.Global_get.make wasm_mod "heap_top" Type.int32) Type.int32)
            (Expression.Local_get.make wasm_mod 0 Type.int32))
      ; Expression.Local_get.make wasm_mod 1 Type.int32 ]
  in
  ignore @@
    Function.add_function wasm_mod "malloc" Type.int32 Type.int32 [| Type.int32 |] body

let compile_ir ~memory ~optimize ~debug ~shared_memory wasm_mod contract =
  let IR_of_michelson.{ main; lambdas; static_data; _ } = contract in
  add_function wasm_mod "main" main;

  if lambdas <> [] then
    begin
      List.iter
        (fun (idx, fn) ->
          add_function wasm_mod (Printf.sprintf "lambda_%d" idx) fn)
        lambdas;
      compile_exec_function wasm_mod lambdas;
    end;

  compile_malloc wasm_mod;
  ignore @@ Export.add_function_export wasm_mod "malloc" "malloc";

  ignore @@
    Global.add_global wasm_mod "__michelson_stack" Type.int32 true
      (Expression.Const.make wasm_mod (Literal.int32 0l));  
  ignore @@
    Global.add_global wasm_mod "heap_top" Type.int32 true
      (Expression.Const.make wasm_mod (Literal.int32 512l));

  ignore @@
    Global.add_global wasm_mod "__michelson_dip_stack" Type.int32 true
      (Expression.Const.make wasm_mod (Literal.int32 256l));

  if debug then begin
    ignore @@ Export.add_global_export wasm_mod "__michelson_stack" "__michelson_stack";
    ignore @@ Export.add_global_export wasm_mod "heap_top" "heap_top";
  end;

  Import.add_function_import wasm_mod "parameter_size" "env" "parameter_size" Type.none Type.int32;
  Import.add_function_import wasm_mod "parameter_load" "env" "parameter_load" Type.int32 Type.int32;
  Import.add_function_import wasm_mod "save_storage" "env" "save_storage" Type.(create [| int32; int32 |]) Type.int32;
  Import.add_function_import wasm_mod "failwith" "env" "failwith" Type.int32 Type.none;

  Import.add_function_import wasm_mod "sender" "env" "sender" Type.none Type.int32;
  Import.add_function_import wasm_mod "amount" "env" "amount" Type.none Type.int32;
  Import.add_function_import wasm_mod "transfer_tokens" "env" "transfer_tokens" Type.(create [| int32; int32; int32|]) Type.int32;

  Import.add_function_import wasm_mod "lookup_address" "env" "lookup_address" Type.int32 Type.int32;
  Import.add_function_import wasm_mod "reverse_lookup_address" "env" "reverse_lookup_address" Type.int32 Type.int32;

  Import.add_function_import wasm_mod "michelson_dig" "env" "michelson_dig" Type.int32 Type.int32;

  let (initial, max) = memory in
  let segments =
    [ Memory.{ data = static_data
             ; kind = Active { offset = Expression.Const.make wasm_mod (Literal.int32 0l) }
             ; size = Bytes.length static_data } ] in
  Memory.set_memory wasm_mod initial max "memory" segments shared_memory;

  (* if Module.validate wasm_mod <> 0 then
    failwith "Generated module is invalid"; *)

  if optimize then
    Module.optimize wasm_mod;

  wasm_mod