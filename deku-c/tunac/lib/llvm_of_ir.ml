open IR

let reg_count = ref 0
let new_reg () =
  incr reg_count;
  Printf.sprintf "%%r%d" !reg_count

let label_count = ref 0
let new_label () =
  incr label_count;
  Printf.sprintf "L%d" !label_count

let compile_wasm_operation output typ op args =
  let typ =
    match typ with
    | I32 -> "i32"
    | U32 -> "u32"
    | I8 -> "i8"
    | U8 -> "u8"
  in

  let op2 op a b =
    let a' = new_reg () in
    let b' = new_reg () in
    let c' = new_reg () in
    let c'' = new_reg () in
    Format.fprintf output "\t%s = ptrtoint ptr %s to %s\n" a' a typ;
    Format.fprintf output "\t%s = ptrtoint ptr %s to %s\n" b' b typ;
    Format.fprintf output "\t%s = %s %s %s, %s\n" c' op typ a' b';
    Format.fprintf output "\t%s = inttoptr %s %s to ptr\n" c'' typ c';
    c''
  in

  let cmp op a b =
    let reg = new_reg () in
    let reg' = new_reg () in
    let reg'' = new_reg () in
    Format.fprintf output "\t%s = icmp %s ptr %s, %s\n" reg op a b;
    Format.fprintf output "\t%s = select i1 %s, i32 1, i32 0\n" reg' reg;
    Format.fprintf output "\t%s = inttoptr i32 %s to ptr\n" reg'' reg';
    reg''
  in

  match op, args with
  | Wasm_add, [ a; b ] -> op2 "add" a b
  | Wasm_sub, [ a; b ] -> op2 "sub" a b
  | Wasm_mul, [ a; b ] -> op2 "mul" a b
  | Wasm_div, [ a; b ] -> op2 "div" a b
  | Wasm_rem, [ a; b ] -> op2 "rem" a b
  | Wasm_shr, [ a; b ] -> op2 "lshr" a b
  | Wasm_xor, [ a; b ] -> op2 "xor" a b
  | Wasm_and, [ a; b ] -> op2 "and" a b

  | Wasm_gt, [ a; b ] -> cmp "sgt" a b
  | Wasm_lt, [ a; b ] -> cmp "slt" a b
  | Wasm_eq, [ a; b ] -> cmp "eq" a b
  | Wasm_eqz, [ a ] -> cmp "eq" a "null"

  | _ -> Format.printf "Unsupported operation %a\n" pp_wasm_operation op; assert false

let rec compile_expression output expr =
  match expr with
  | Cvar local ->
    let r = new_reg () in
    Format.fprintf output "\t%s = load ptr, ptr %%local_%d\n" r local;  
    r

  | Cglobal name ->
    let r = new_reg () in
    Format.fprintf output "\t%s = load ptr, ptr @%s\n" r name;
    r

  | Cconst_i32 value ->
    let reg = new_reg () in
    Format.fprintf output "\t%s = inttoptr i32 %ld to ptr\n" reg value;
    reg

  | Cop (Cload (cell, _), [ ptr ]) ->
    let ptr = compile_expression output ptr in
    let tmp = new_reg () in
    let value = new_reg () in
    Format.fprintf output "\t%s = getelementptr ptr, ptr %s, i32 %d\n" tmp ptr cell;
    Format.fprintf output "\t%s = load ptr, ptr %s\n" value tmp;
    value

  | Cop (Calloc 0, [ size ]) ->
    let size = compile_expression output size in
    let size' = new_reg () in
    let ptr = new_reg () in
    Format.fprintf output "\t%s = ptrtoint ptr %s to i32\n" size' size; 
    Format.fprintf output "\t%s = call ptr @malloc(i32 %s)\n" ptr size';
    ptr

  | Cop (Calloc cells, []) ->
    let ptr = new_reg () in
    Format.fprintf output "\t%s = call ptr @malloc(i32 %d)\n" ptr (cells * 4);
    ptr

  | Cop (Capply name, args) ->
    let args =
      args
      |> List.map (fun expr -> Printf.sprintf "ptr %s" (compile_expression output expr))
      |> String.concat ", "
    in
    let reg = new_reg () in
    Format.fprintf output "\t%s = call ptr @%s(%s)\n" reg name args;
    reg

  | Cop (Cwasm (op, typ), args) ->
    let args = List.map (compile_expression output) args in
    compile_wasm_operation output typ op args

  | _ ->
    Format.fprintf output "%a\n" IR.pp_expression expr;
    assert false

let is_block = function Cblock _ -> true | _ -> false

let while_stack = ref []

let rec compile_statement output statement =
  Format.pp_print_newline output ();

  (* if (not (is_block statement)) then
    Format.printf "%a\n" pp_statement statement; *)

  match statement with
  | Cassign (local, value) ->
    let ptr = compile_expression output value in
    Format.fprintf output "\tstore ptr %s, ptr %%local_%d\n" ptr local

  | Cglobal_assign (global, expr) ->
    let value = compile_expression output expr in
    Format.fprintf output "\tstore ptr %s, ptr @%s\n" value global

  | Cstore (cell, ptr, value) ->
    let value = compile_expression output value in
    let ptr = compile_expression output ptr in
    let reg = new_reg () in
    Format.fprintf output "\t%s = getelementptr ptr, ptr %s, i32 %d\n" reg ptr cell;
    Format.fprintf output "\tstore ptr %s, ptr %s\n" value reg

  | Cblock statements -> List.iter (compile_statement output) statements

  | Cifthenelse (condition, then_branch, else_branch) ->
    let condition =
      let cond = compile_expression output condition in
      let reg = new_reg () in
      Format.fprintf output "\t%s = icmp ne ptr %s, null\n" reg cond;
      reg
    in
    let else_label = new_label () in
    let then_label = new_label () in
    
    Format.fprintf output "\tbr i1 %s, label %%%s, label %%%s\n" condition then_label else_label;
    
    Format.fprintf output "%s: ; else\n" else_label;
    compile_statement output else_branch;

    let end_label = new_label () in
    Format.fprintf output "\tbr label %%%s\n" end_label;

    Format.fprintf output "%s: ; then\n" then_label;
    compile_statement output then_branch;

    Format.fprintf output "\tbr label %%%s\n" end_label;

    Format.fprintf output "%s: ; end\n" end_label;

  | Cwhile (condition, body) ->
    let while_label = new_label () in
    Format.fprintf output "\tbr label %%%s\n" while_label;
    Format.fprintf output "%s: ; while label\n" while_label;
    while_stack := while_label :: !while_stack;
    let condition =
      let cond = compile_expression output condition in
      let reg = new_reg () in
      Format.fprintf output "\t%s = icmp ne ptr %s, null\n" reg cond;
      reg
    in
    let end_label = new_label () in
    let body_label = new_label () in
    Format.fprintf output "\tbr i1 %s, label %%%s, label %%%s\n" condition body_label end_label;
    Format.fprintf output "%s: ; body\n" body_label;
    compile_statement output body;
    Format.fprintf output "\tbr label %%%s\n" while_label;
    Format.fprintf output "%s: ; while end\n" end_label;
    while_stack := List.tl !while_stack

  | Ccontinue ->
    let label = List.hd !while_stack in
    Format.fprintf output "\tbr label %s\n" label

  | Cfailwith failure ->
    let failure = compile_expression output failure in
    Format.fprintf output "\tcall void @failwith(ptr %s)\n" failure;
    Format.fprintf output "\tunreachable\n"

let compile_function_body output arguments body locals =
  for i = 0 to locals do
    Format.fprintf output "\t%%local_%d = alloca ptr\n" i;
  done;
  for i = 0 to arguments - 1 do
    Format.fprintf output "\tstore ptr %%%d, ptr %%local_%d\n" i i;
  done;
  compile_statement output body

let compile_function output name fn =
  Format.fprintf output "\ndefine void @%s() {\n" name;
  let IR_of_michelson.{ body; locals } = fn in
  compile_function_body output 0 body locals;
  Format.fprintf output "\tret void\n}"

let compile_compare_function output name ret fn =
  Format.fprintf output "\ndefine ptr @%s(ptr %%0, ptr %%1) {\n" name;
  let IR_of_michelson.{ body; locals } = fn in

  compile_function_body output 2 body locals;
  let ret_reg = new_reg () in
  Format.fprintf output "\t%s = load ptr, ptr %%local_%d\n" ret_reg ret;
  Format.fprintf output "\tret ptr %s\n }" ret_reg

let compile_main_compare_function output functions =
  let functions = List.rev functions in
  List.iteri
    (fun idx (ret, fn) ->
      let name = Printf.sprintf "michelson_compare_function_%d" idx in
      compile_compare_function output name ret fn)
    functions;

  Format.fprintf output "\ndefine i32 @michelson_dynamic_compare(ptr %%0, ptr %%1, ptr %%2) {\n";

  let return_value = new_reg () in
  Format.fprintf output "\n%s = alloca i32\n" return_value;
  let switch = new_label () in
  let default = new_label () in
  let key = new_reg () in
  Format.fprintf output "br label %%%s\n" switch;
  Format.fprintf output "%s:\n" switch;
  Format.fprintf output "\t%s = ptrtoint ptr %%0 to i32\n" key;
  Format.fprintf output "\tswitch i32 %s, label %%%s [\n" key default;
  List.iteri
      (fun idx _ ->
        Format.fprintf output "\t\ti32 %d, label %%branch_%d\n" idx idx)
      functions;

  Format.fprintf output "\t]\n";
  Format.fprintf output "%s:\n" default;
  Format.fprintf output "\tunreachable\n";

  let return_point = new_label () in

  List.iteri
        (fun idx _ ->
          let value = new_reg () in
          Format.fprintf output "\nbranch_%d:\n" idx;
          Format.fprintf output "\t%s = call i32 @michelson_compare_function_%d(ptr %%1, ptr %%2)\n" value idx;
          Format.fprintf output "\tstore i32 %s, ptr %s\n" value return_value;
          Format.fprintf output "\tbr label %%%s\n" return_point)
        functions;

  Format.fprintf output "%s:\n" return_point;
  let value = new_reg () in
  Format.fprintf output "\t%s = load i32, ptr %s\n" value return_value;
  Format.fprintf output "\tret i32 %s\n" value;
  Format.fprintf output "}\n"

let compile_ir output contract =
  let IR_of_michelson.{ main; compare; _ } = contract in

  Format.fprintf output "@__michelson_stack = global ptr null\n";
  Format.fprintf output "declare ptr @malloc(i32)\n";
  Format.fprintf output "declare ptr @parameter_load(ptr)\n";
  Format.fprintf output "declare ptr @parameter_size()\n";
  Format.fprintf output "declare ptr @save_storage(ptr, ptr)\n";
  Format.fprintf output "declare ptr @failwith(ptr)\n";
  Format.fprintf output "declare ptr @lookup_address(ptr)\n";
  Format.fprintf output "declare ptr @reverse_lookup_address(ptr)\n";
  Format.fprintf output "declare ptr @sender()\n";
  Format.fprintf output "declare ptr @transfer_tokens(ptr, ptr, ptr)\n";

  Format.fprintf output "declare void @michelson_dup_n(ptr)\n";
  Format.fprintf output "declare void @michelson_drop_n(ptr)\n";
  Format.fprintf output "declare void @michelson_dug_n(ptr)\n";
  Format.fprintf output "declare void @michelson_dig_n(ptr)\n";

  Format.fprintf output "declare ptr @michelson_map_get(ptr, ptr)\n";
  Format.fprintf output "declare ptr @michelson_map_update(ptr, ptr, ptr)\n";

  (* TODO: Remove these while we don't have a better design for logging *)
  Format.fprintf output "declare void @writev(ptr)\n";
  Format.fprintf output "declare void @inspect_stack()\n";


  (* TODO: add lambdas *)
  Format.fprintf output "define ptr @exec(ptr %%0) { ret ptr null }\n";

  compile_main_compare_function output compare;
  compile_function output "main" main

let compile_llvm_to_wasm input output =
  let ret = Sys.command ("llc -o " ^ output ^ " --march=wasm32 --filetype=obj -opaque-pointers " ^ input) in
  match ret with
  | 0 -> ()
  | _ ->
    ignore @@ Sys.command ("cp " ^ input ^ " /tmp/failed.ll");
    failwith "Couldn't compile LLVM module"