open Core_bench
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

  let test name f = Bench.Test.create f ~name in
  let stack = Array.make 51000 0 in
  [
   
    test "array zero_alloc eval" (fun _ ->
        Vm.intepret s ~debug:false ~remaining_gas:max_int ~code ~stack:[||]);
    test "array direct toy zero alloc" (fun _ ->
        Counter_toy_no_alias.interpret 0 stack 0 Counter_toy_no_alias.code);
  ]

let () = tests () |> Bench.make_command |> Core.Command.run
