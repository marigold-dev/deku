open Base
open Core_bench
open Zinc_interpreter.Dummy
open Ir.Zt.Zinc

let convert zinc =
  let from =
    List.nth_exn zinc (List.length zinc - 1) |> snd |> Ir.Zt.Zinc.to_yojson
  in
  let to_ = Ir.Zt.Zinc.of_yojson from |> Result.ok_or_failwith in
  to_

open Z

let prog1 =
  let list =
    List.init 100000 ~f:(fun x -> [Plain_old_data (Mutez ~$x); Operation Cons])
    |> List.concat
  in
  [("a", List.concat [[Plain_old_data Nil]; list; [Core Return]])] |> convert

let prog2 =
  [
    ( "or_true",
      [
        Core Grab;
        Plain_old_data (Bool true);
        Core (Access 0);
        Operation Or;
        Core Return;
      ] );
    ( "or_false",
      [
        Core
          (Closure
             [
               Core Grab;
               Plain_old_data (Bool true);
               Core (Access 0);
               Operation Or;
               Core Return;
             ]);
        Core Grab;
        Core Grab;
        Plain_old_data (Bool false);
        Core (Access 0);
        Operation Or;
        Core Return;
      ] );
    ( "and_true",
      [
        Core
          (Closure
             [
               Core Grab;
               Plain_old_data (Bool true);
               Core (Access 0);
               Operation Or;
               Core Return;
             ]);
        Core Grab;
        Core
          (Closure
             [
               Core Grab;
               Plain_old_data (Bool false);
               Core (Access 0);
               Operation Or;
               Core Return;
             ]);
        Core Grab;
        Core Grab;
        Plain_old_data (Bool true);
        Core (Access 0);
        Operation And;
        Core Return;
      ] );
    ( "and_false",
      [
        Core
          (Closure
             [
               Core Grab;
               Plain_old_data (Bool true);
               Core (Access 0);
               Operation Or;
               Core Return;
             ]);
        Core Grab;
        Core
          (Closure
             [
               Core Grab;
               Plain_old_data (Bool false);
               Core (Access 0);
               Operation Or;
               Core Return;
             ]);
        Core Grab;
        Core
          (Closure
             [
               Core Grab;
               Plain_old_data (Bool true);
               Core (Access 0);
               Operation And;
               Core Return;
             ]);
        Core Grab;
        Core Grab;
        Plain_old_data (Bool false);
        Core (Access 0);
        Operation And;
        Core Return;
      ] );
    ( "not_bool",
      [
        Core
          (Closure
             [
               Core Grab;
               Plain_old_data (Bool true);
               Core (Access 0);
               Operation Or;
               Core Return;
             ]);
        Core Grab;
        Core
          (Closure
             [
               Core Grab;
               Plain_old_data (Bool false);
               Core (Access 0);
               Operation Or;
               Core Return;
             ]);
        Core Grab;
        Core
          (Closure
             [
               Core Grab;
               Plain_old_data (Bool true);
               Core (Access 0);
               Operation And;
               Core Return;
             ]);
        Core Grab;
        Core
          (Closure
             [
               Core Grab;
               Plain_old_data (Bool false);
               Core (Access 0);
               Operation And;
               Core Return;
             ]);
        Core Grab;
        Core Grab;
        Core (Access 0);
        Operation Not;
        Core Return;
      ] );
  ]
  |> convert

let prog2_stack = [Ir.Zt.Stack_item.Z (Plain_old_data (Bool false))]

let prog3 =
  [
    ("a", [Plain_old_data (Num ~$2); Core Return]);
    ( "b",
      [
        Plain_old_data (Num ~$2);
        Core Grab;
        Plain_old_data (Num ~$3);
        Core Return;
      ] );
    ( "lf",
      [
        Plain_old_data (Num ~$2);
        Core Grab;
        Plain_old_data (Num ~$3);
        Core Grab;
        Plain_old_data (Bool true);
        Plain_old_data (Bool false);
        Operation Or;
        Core Grab;
        Core (Access 0);
        Adt
          (MatchVariant
             [|
               [Core Grab; Core (Access 2); Core Return];
               [Core Grab; Core (Access 3); Core Return];
             |]);
      ] );
  ]
  |> convert

module Executor : Zinc_interpreter.Dummy.Executor = struct
  let get_contract_opt a = Some (a, None)

  let chain_id = "chain id goes here"

  let hash = Fn.id

  let key_hash s = s ^ "hash"
end

open Caml

let code =
  let open Vm in
  let l =
    List.init 25000 (fun _ -> 1) |> List.concat_map (fun _ -> [IADD; CONST 1])
  in
  [[CONST 1; CONST 1]; l; [IADD; RETURN]] |> List.flatten |> Array.of_list

let code2 =
  Vm.
    [
      CONST 0;
      BRANCHIFNOT 4;
      ACCESS 1;
      CONST 1;
      IADD;
      RETURN;
      CLOSURE 1;
      CONST 2;
      APPLY;
      RETURN;
    ]
  |> Array.of_list

(* (lambda x -> if x = 0 then 1 else 0) 2 *)
let code_branching =
  Vm.
    [
      CONST 0;
      BRANCHIFNOT 8;
      ACCESS 1;
      CONST 0;
      EQINT;
      BRANCHIFNOT 2;
      CONST 1;
      RETURN;
      CONST 0;
      RETURN;
      CLOSURE 2;
      CONST 2;
      APPLY;
      RETURN;
    ]
  |> Array.of_list

let code_str =
  Vm.
    [
      CONSTBYTES ("Test" |> Bytes.of_string);
      CONSTBYTES ("Test" |> Bytes.of_string);
      CONSTBYTES ("Test" |> Bytes.of_string);
      CONSTBYTES ("Test" |> Bytes.of_string);
      CONSTBYTES ("Test" |> Bytes.of_string);
      RETURN;
    ]
  |> Array.of_list

let s = Vm.make_default ()

module Counter_toy_no_alias = struct
  type instr = IADDD | PUSH of int | HALT

  type instrs = instr array

  let code =
    let l = List.init 25000 (fun _ -> 1) in
    let l = List.concat_map (fun x -> [IADDD; PUSH x]) l in
    Array.of_list ([PUSH 1; PUSH 1] @ l @ [HALT])

  exception Out_of_bound_write

  exception Out_of_bound_read

  let[@ocaml.inline] read (arr : int array) pointer : int =
    Array.get arr pointer

  let[@ocaml.inline] read2 (arr : instr array) pointer = Array.get arr pointer

  let[@ocaml.inline] write (arr : int array) pointer (data : int) =
    Array.set arr pointer data

  let rec interpret stack_pointer stack instr_pointer instrs =
    let instr = read2 instrs instr_pointer in
    let instr_pointer = instr_pointer + 1 in
    match instr with
    | IADDD ->
        let fst = read stack stack_pointer in
        let stack_pointer = stack_pointer - 1 in
        let snd = read stack stack_pointer in
        let value = fst + snd in
        write stack stack_pointer value ;
        interpret stack_pointer stack instr_pointer instrs
    | PUSH n ->
        let stack_pointer = stack_pointer + 1 in
        write stack stack_pointer n ;
        interpret stack_pointer stack instr_pointer instrs
    | HALT -> ()
end

let tests () =
  (* let runner1 = Interpreter.eval (module Executor) in *)
  (* let runner2 = Interpreter.eval' (module Executor) ~debug:false in
     (* let prog1 = Interpreter.initial_state prog1 in
        let prog2' = Ir.of_typed prog2 in
        let prog2' = (prog2', [], Ir.of_typed_stack prog2_stack) in
        let prog2 = Interpreter.initial_state ~initial_stack:prog2_stack prog2 in
        let prog3' = Ir.of_typed prog3 in
        let prog3' = (prog3', [], []) in
        let prog3 = Interpreter.initial_state prog3 in *)
     let progn2 : Ir.t list =
       [
         Ir.Num Z.one;
         Ir.Num (Z.of_int 3);
         Ir.Add;
         Ir.Num (Z.of_int 3);
         Ir.Add;
         Ir.Num (Z.of_int 3);
         Ir.Add;
         Ir.Num (Z.of_int 3);
         Ir.Add;
         Ir.Num (Z.of_int 3);
         Ir.Add;
         Ir.Num (Z.of_int 3);
         Ir.Add;
         Ir.Num (Z.of_int 3);
         Ir.Add;
         Ir.Num (Z.of_int 3);
         Ir.Add;
         Ir.Num (Z.of_int 3);
         Ir.Add;
         Ir.Num (Z.of_int 3);
         Ir.Add;
         Ir.Num (Z.of_int 3);
         Ir.Add;
         Ir.Num (Z.of_int 3);
         Ir.Add;
         Ir.Num (Z.of_int 3);
         Ir.Add;
         Ir.Num (Z.of_int 3);
         Ir.Add;
         Ir.Num (Z.of_int 3);
         Ir.Add;
         Ir.Return;
       ]
     in
     let progn2 = (progn2, [], []) in *)
  (* funcall \x -> x + 1 *)
  (* let _ = runloop s2 in
     let block = Stack.pop s2.stack in
     let vale = Memory.read_field s2.memory ~field:1 ~block in
     Printf.printf "resutl is %d" vale ;
     Stack.set_pointer s2.T.stack ~value:0 ;
     Progn.set_pointer s2.T.program ~value:0 ;
     Memory.set_pointer s2.T.memory ~value:0 ;
     if true then failwith "test" else () ; *)
  let test name f = Bench.Test.create f ~name in
  let stack = Array.make 51000 0 in
  [
    (* test "old_eval list_cons" (fun _ -> runner1 prog1);
       test "new_eval list_cons" (fun _ -> runner2 prog1');
       test "old_eval bools" (fun _ -> runner1 prog2);
       test "new_eval bools" (fun _ -> runner2 prog2');
       test "old_eval if_then_else" (fun _ -> runner1 prog3);
       test "new_eval if_then_else" (fun _ -> runner2 prog3'); *)
    test "array zero_alloc eval" (fun _ ->
        Vm.intepret s ~debug:false ~remaining_gas:max_int ~code ~stack:[||]);
    test "array direct toy zero alloc" (fun _ ->
        Counter_toy_no_alias.interpret 0 stack 0 Counter_toy_no_alias.code);
  ]

let () = tests () |> Bench.make_command |> Core.Command.run
