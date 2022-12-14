open IR

let reg_count = ref 0
let new_reg () =
  incr reg_count;
  Printf.sprintf "%%%d" !reg_count

let compile_wasm_operation op args =
  match op, args with
  | Wasm_add, [ a; b ] ->
    let a' = new_reg () in
    let b' = new_reg () in
    let c' = new_reg () in
    let c'' = new_reg () in
    Format.printf "\t%s = ptrtoint ptr %s to i32\n" a' a; 
    Format.printf "\t%s = ptrtoint ptr %s to i32\n" b' b;
    Format.printf "\t%s = add i32 %s, %s\n" c' a' b';
    Format.printf "\t%s = inttoptr i32 %s to ptr\n" c'' c';
    c''

  | _ -> assert false

let rec compile_expression expr =
  match expr with
  | Cvar local ->
    let r = new_reg () in
    Format.printf "\t%s = load ptr, ptr %%local_%d\n" r local;  
    r

  | Cglobal name ->
    let r = new_reg () in
    Format.printf "\t%s = load ptr, ptr @%s\n" r name;
    r

  | Cconst_i32 value ->
    let reg = new_reg () in
    Format.printf "\t%s = inttoptr i32 %ld to ptr\n" reg value;
    reg

  | Cop (Cload (cell, _), [ ptr ]) ->
    let ptr = compile_expression ptr in
    let tmp = new_reg () in
    let value = new_reg () in
    Format.printf "\t%s = getelementptr ptr, ptr %s, i32 %d\n" tmp ptr cell;
    Format.printf "\t%s = load ptr, ptr %s\n" value tmp;
    value

  | Cop (Calloc 0, [ size ]) ->
    let size = compile_expression size in
    let size' = new_reg () in
    let ptr = new_reg () in
    Format.printf "\t%s = ptrtoint ptr %s to i32\n" size' size; 
    Format.printf "\t%s = call ptr @malloc(i32 %s)\n" ptr size';
    ptr

  | Cop (Calloc cells, []) ->
    let ptr = new_reg () in
    Format.printf "\t%s = call ptr @malloc(i32 %d)\n" ptr (cells * 4);
    ptr

  | Cop (Capply name, args) ->
    let args =
      args
      |> List.map (fun expr -> Printf.sprintf "ptr %s" (compile_expression expr))
      |> String.concat ", "
    in
    let reg = new_reg () in
    Format.printf "\t%s = call ptr @%s(%s)\n" reg name args;
    reg

  | Cop (Cwasm (op, _), args) ->
    let args = List.map compile_expression args in
    compile_wasm_operation op args

  | _ ->
    Format.printf "%a\n" IR.pp_expression expr;
    assert false

let is_block = function Cblock _ -> true | _ -> false

let rec compile_statement statement =
  Format.print_newline ();

  (* if (not (is_block statement)) then
    Format.printf "%a\n" pp_statement statement; *)

  match statement with
  | Cassign (local, value) ->
    let ptr = compile_expression value in
    Format.printf "\tstore ptr %s, ptr %%local_%d\n" ptr local

  | Cglobal_assign (global, expr) ->
    let value = compile_expression expr in
    Format.printf "\tstore ptr %s, ptr @%s\n" value global

  | Cstore (cell, ptr, value) ->
    let value = compile_expression value in
    let ptr = compile_expression ptr in
    let reg = new_reg () in
    Format.printf "\t%s = getelementptr ptr, ptr %s, i32 %d\n" reg ptr cell;
    Format.printf "\tstore ptr %s, ptr %s\n" value reg

  | Cblock statements -> List.iter compile_statement statements

  | _ ->
    Format.printf "%a\n" IR.pp_statement statement;
    assert false

let compile_function name fn =
  Format.printf "\ndefine void @%s() {\n" name;

  let IR_of_michelson.{ body; locals } = fn in

  for i = 0 to locals do
    Format.printf "\t%%local_%d = alloca ptr\n" i;
  done;

  compile_statement body;

  Format.printf "\tret void\n}"

let compile_ir contract =
  let IR_of_michelson.{ main; _ } = contract in

  Format.printf "@__michelson_stack = global ptr null\n";
  Format.printf "declare ptr @malloc(i32)\n";
  Format.printf "declare ptr @parameter_load(ptr)\n";
  Format.printf "declare ptr @parameter_size()\n";
  Format.printf "declare ptr @save_storage(ptr, ptr)\n";

  compile_function "main" main;