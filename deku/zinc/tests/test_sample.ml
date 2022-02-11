[@@@warning "-40"]

open Zinc_interpreter.Dummy
module Zinc_types = Ir.Zt

let zinc =
  Zinc_types.Zinc.
    [
      Plain_old_data (Address "tz1TGu6TN5GSez2ndXXeDX6LgUDvLzPLqgYV");
      Core Grab;
      Core (Access 0);
      Domain_specific_operation Contract_opt;
      Core Grab;
      Core (Access 0);
      Adt
        (MatchVariant
           [|
             [Core Grab; Core (Access 0)];
             [
               Core Grab;
               Plain_old_data (String "Not a contract");
               Control_flow Failwith;
             ];
           |]);
      Core EndLet;
      Core Grab;
      Plain_old_data (String "my string");
      Plain_old_data
        (Key "edpkuBknW28nW72KG6RoHtYW7p12T6GKc7nAbwYX5m8Wd9sDVC9yav");
      Core (Access 0);
      Plain_old_data (Mutez (Z.of_int 10));
      Adt (MakeRecord 0);
      Domain_specific_operation MakeTransaction;
      Adt (MakeRecord 3);
      Core Return;
    ]
  |> Zinc_types.Zinc.to_yojson

module Executor : Zinc_interpreter.Dummy.Executor = struct
  let get_contract_opt a = Some (a, None)

  let chain_id = "chain id goes here"

  let hash = Fun.id

  let key_hash s = s ^ "hash"
end

let%test _ =
  let open Zinc_interpreter.Dummy in
  let code = Ir.Num Z.one in
  let written = Bin_prot.Utils.bin_dump Ir.bin_writer_t code in
  let[@warning "-8"] (Ir.Num x) = Ir.bin_read_t ~pos_ref:(ref 0) written in
  Z.equal x Z.one

let%test _ =
  let open Zinc_interpreter.Dummy in
  let code = Ir.List [Ir.String "test"; Ir.Num Z.one] in
  let code2 = Ir.String "test" in
  not (Int.equal (Ir.bin_size_t code) (Ir.bin_size_t code2))

let%expect_test _ =
  let open Zinc_interpreter.Dummy in
  let open Interpreter in
  let zinc = Zinc_types.Zinc.of_yojson zinc |> Result.get_ok |> Ir.of_typed in
  let env, stack = eval' (module Executor) ~debug:false (zinc, [], []) in
  let str =
    Format.asprintf
      "Env: %s\nStack:%s\n"
      ([%derive.show: Ir.t list] env)
      ([%derive.show: Ir.t list] stack)
  in
  print_endline str ;
  [%expect
    {|
    Env: [(Contract ["tz1TGu6TN5GSez2ndXXeDX6LgUDvLzPLqgYV",null]);
      Variant {tag = 0;
        value = (Contract ["tz1TGu6TN5GSez2ndXXeDX6LgUDvLzPLqgYV",null])};
      (Address tz1TGu6TN5GSez2ndXXeDX6LgUDvLzPLqgYV)]
    Stack:[(Record
        [|(Transaction (10, ["tz1TGu6TN5GSez2ndXXeDX6LgUDvLzPLqgYV",null]));
          (Key edpkuBknW28nW72KG6RoHtYW7p12T6GKc7nAbwYX5m8Wd9sDVC9yav);
          (String "my string")|])
      ] |}]

let%expect_test _ =
  let open Zinc_interpreter.Dummy in
  let open Interpreter in
  let zinc = Zinc_types.Zinc.of_yojson zinc |> Result.get_ok in
  let run_resutl = eval (module Executor) (initial_state zinc) in
  Zinc_types.Interpreter_output.to_string run_resutl |> print_endline ;
  [%expect
    {|
             (Success (
                [(NonliteralValue (Contract ["tz1TGu6TN5GSez2ndXXeDX6LgUDvLzPLqgYV",null]));
                  (Variant (0,
                     (NonliteralValue
                        (Contract ["tz1TGu6TN5GSez2ndXXeDX6LgUDvLzPLqgYV",null]))
                     ));
                  (Z (Plain_old_data (Address tz1TGu6TN5GSez2ndXXeDX6LgUDvLzPLqgYV)))],
                [(Record
                    [|(NonliteralValue
                         (Chain_operation
                            (Transaction (10,
                               ["tz1TGu6TN5GSez2ndXXeDX6LgUDvLzPLqgYV",null]))));
                      (Z
                         (Plain_old_data
                            (Key edpkuBknW28nW72KG6RoHtYW7p12T6GKc7nAbwYX5m8Wd9sDVC9yav)));
                      (Z (Plain_old_data (String "my string")))|])
                  ]
                )) |}]

(* module T = struct
     exception Out_of_bound_write

     exception Out_of_bound_read

     exception Type_error

     (* TODO: use GADTs and proper fetch and decode to avoid allocations *)
     (* https://xavierleroy.org/mpri/2-4/machines.pdf SECD *)
     type instr =
       (* get nth position of env *)
       | ACCESS of int
       (* move top of the stack to env *)
       | LET
       (* drop top of the env *)
       | ENDLET
       | CLOSURE of instr array
       | APPLY
       | TAILAPPLY
       | RETURN
       | CONST of Obj.t
       | IADD
       | SCONCAT

     (* TODO: maybe use Obj.magic in the future to remove fetch_and_decode *)

     (* TODO: this doesn't work on OCaml 32 bits*)
     type imm_32 = int

     type item = Obj.t

     type program = instr array

     (* TODO: maybe stack should be bigarray to avoid being scanned by GC *)
     type stack = item array

     type env = stack

     type pointer = imm_32

     (* TODO: should we allow JIT? *)
     type state = {
       mutable program_counter : pointer;
       program : program;
       mutable stack_pointer : pointer;
       stack : stack;
       mutable env_pointer : pointer;
       env : env;
       mutable gas_counter : int; (* TODO: proper abstract type here *)
     }

     let[@inline always] read memory pointer =
       try Array.get memory pointer
       with Invalid_argument _ -> raise Out_of_bound_read

     let[@inline always] write memory pointer value =
       try Array.set memory pointer value
       with Invalid_argument _ -> raise Out_of_bound_write

     let push_stack state value =
       let stack = state.stack in
       let stack_pointer = state.stack_pointer + 1 in

       write stack state.stack_pointer value ;
       state.stack_pointer <- stack_pointer

     let pop_stack state =
       let stack = state.stack in
       let stack_pointer = state.stack_pointer - 1 in
       let value = read stack stack_pointer in
       state.stack_pointer <- stack_pointer ;
       value

     let push_env state value =
       let env = state.env in
       let env_pointer = state.env_pointer + 1 in

       write env env_pointer value ;
       state.env_pointer <- env_pointer

     let tail_env state =
       let env_pointer = state.env_pointer - 1 in
       state.env_pointer <- env_pointer

     let access_env state ~offset =
       let env = state.env in
       let offset = state.env_pointer - offset in
       read env offset

     (* TODO: adding an accu would probably be faster *)

     let verify_gas state = state.gas_counter > 0

     let fetch_and_decode state =
       (* TODO: is Array.length compare + unsafe_get + raise faster? *)
       let program = state.program in
       let program_counter = state.program_counter in
       try Array.get program program_counter
       with Invalid_argument _ -> raise Out_of_bound_read

     (* let execute state instr =
        match instr with
        | ACCESS offset ->
            let value = access_env state ~offset in
            push_stack state value
        | LET ->
            let value = pop_stack state in
            push_env state value
        | ENDLET -> tail_env state
        | CLOSURE offset ->
            (* TODO: should this be absolute? *)
            let env_pointer = state.env_pointer in
            (* TODO: check if storing only the env pointer z is safe *)
            let program_counter = state.program_counter + offset in
            let closure = Closure {env_pointer; program_counter} in
            push_stack state closure
        | APPLY -> (
            let value = pop_stack state in
            let closure = pop_stack state in

            match closure with
            | Int _ -> raise Type_error
            | Closure {env_pointer; program_counter} ->
                push_stack state (Int state.env_pointer) ;
                push_stack state (Int state.program_counter) ;

                state.env_pointer <- env_pointer ;
                push_env state value ;

                state.program_counter <- program_counter)
        | TAILAPPLY -> (
            let value = pop_stack state in
            let closure = pop_stack state in

            match closure with
            | Int _ -> raise Type_error
            | Closure {env_pointer; program_counter} ->
                state.env_pointer <- env_pointer ;
                push_env state value ;

                state.program_counter <- program_counter)
        | RETURN -> (
            let value = pop_stack state in
            let program_counter = pop_stack state in
            let env_pointer = pop_stack state in

            match (program_counter, env_pointer) with
            | (Closure _, _) | (_, Closure _) -> raise Type_error
            | (Int program_counter, Int env_pointer) ->
                push_stack state value ;
                state.env_pointer <- env_pointer ;
                state.program_counter <- program_counter)
        | CONST int -> push_stack state (Int int) *)

     let rec clos cd_pointer code env_pc state =
       state.program_counter <- cd_pointer ;
       state.env_pointer <- env_pc ;
       execute state code

     and execute state code =
       let pc = ref (-1) in
       while true do
         incr pc ;
         Format.printf
           "Stack state => %s,@, pc: %d\n"
           (Array.fold_left
              (fun acc x ->
                let x : int = Obj.obj x in
                acc ^ " " ^ string_of_int x)
              ""
              state.stack)
           state.stack_pointer ;
         match code.(!pc) with
         | ACCESS offset ->
             let value = access_env state ~offset in
             push_stack state value
         | LET ->
             let value = pop_stack state in
             push_env state value
         | ENDLET -> tail_env state
         | CLOSURE offset ->
             push_stack state (Obj.repr (clos 0 offset state.env_pointer))
         | APPLY ->
             let value = pop_stack state in
             let closure' : state -> unit = Obj.obj (pop_stack state) in
             push_stack
               state
               (Obj.repr
                  (clos state.program_counter state.program state.env_pointer)) ;
             push_env state value ;
             closure' state
         | TAILAPPLY ->
             let value = pop_stack state in
             let closure : state -> unit = Obj.obj (pop_stack state) in
             push_env state value ;
             closure state
         | RETURN ->
             let value = pop_stack state in
             let program_counter : int = Obj.obj (pop_stack state) in
             let env_pointer : int = Obj.obj (pop_stack state) in
             push_stack state value ;
             state.env_pointer <- env_pointer ;
             state.program_counter <- program_counter
         | CONST v -> push_stack state v
         | IADD ->
             let int1 = pop_stack state in
             let int2 = pop_stack state in
             let int1 : int = Obj.obj int1 in
             let int2 : int = Obj.obj int2 in
             let res = int1 + int2 in
             push_stack state (Obj.repr res)
         | SCONCAT -> failwith "todo"
       done
   end

   let%expect_test _ =
     let open T in
     let env = Array.init 1000 (fun _ -> Obj.repr 0) in
     let stack = Array.init 10 (fun _ -> Obj.repr 0) in
     let code = [|CONST (Obj.repr 1); CONST (Obj.repr 2); IADD|] in
     let s =
       {
         program_counter = 0;
         program = code;
         stack_pointer = 0;
         stack;
         env_pointer = 0;
         env;
         gas_counter = 0 (* TODO: proper abstract type here *);
       }
     in
     let _ = try execute s code with Invalid_argument _ -> () in
     let res : int = Obj.obj (pop_stack s) in
     Format.printf "result is %d\n" res ;
     [%expect
       {|
       Stack state =>  0 0 0 0 0 0 0 0 0 0,
        pc: 0
       Stack state =>  1 0 0 0 0 0 0 0 0 0,
        pc: 1
       Stack state =>  1 2 0 0 0 0 0 0 0 0,
        pc: 2
       Stack state =>  3 2 0 0 0 0 0 0 0 0,
        pc: 1
       result is 3 |}] *)

module T = struct
  let code =
    let open Vm in
    let l =
      List.init 10 (fun _ -> 1) |> List.concat_map (fun _ -> [IADD; CONST 1])
    in
    [[CONST 1; CONST 1]; l; [IADD; RETURN]] |> List.flatten |> Array.of_list

  (* (lambda x -> x + 1) 2 *)
  let code2 =
    Vm.
      [
        CONST 0;
        BRANCHIFNOT 4;
        ACCESS 1;
        CONST 1;
        IADD;
        RETURN;
        CLOSURE 2;
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

  let s = Vm.default ()

  let%expect_test "trace addition" =
    Vm.intepret
      ~debug:true
      (Vm.default ())
      ~remaining_gas:max_int
      ~code
      ~stack:[||] ;
    [%expect
      {|
      Executing:
        Current Instruction: (CONST 1)
        Current stack: []
        Current env: []
      Executing:
        Current Instruction: (CONST 1)
        Current stack: [(Int 1)]
        Current env: []
      Executing:
        Current Instruction: IADD
        Current stack: [(Int 1); (Int 1)]
        Current env: []
      Executing:
        Current Instruction: (CONST 1)
        Current stack: [(Int 1); (Int 2)]
        Current env: []
      Executing:
        Current Instruction: IADD
        Current stack: [(Int 1); (Int 2)]
        Current env: []
      Executing:
        Current Instruction: (CONST 1)
        Current stack: [(Int 1); (Int 3)]
        Current env: []
      Executing:
        Current Instruction: IADD
        Current stack: [(Int 1); (Int 3)]
        Current env: []
      Executing:
        Current Instruction: (CONST 1)
        Current stack: [(Int 1); (Int 4)]
        Current env: []
      Executing:
        Current Instruction: IADD
        Current stack: [(Int 1); (Int 4)]
        Current env: []
      Executing:
        Current Instruction: (CONST 1)
        Current stack: [(Int 1); (Int 5)]
        Current env: []
      Executing:
        Current Instruction: IADD
        Current stack: [(Int 1); (Int 5)]
        Current env: []
      Executing:
        Current Instruction: (CONST 1)
        Current stack: [(Int 1); (Int 6)]
        Current env: []
      Executing:
        Current Instruction: IADD
        Current stack: [(Int 1); (Int 6)]
        Current env: []
      Executing:
        Current Instruction: (CONST 1)
        Current stack: [(Int 1); (Int 7)]
        Current env: []
      Executing:
        Current Instruction: IADD
        Current stack: [(Int 1); (Int 7)]
        Current env: []
      Executing:
        Current Instruction: (CONST 1)
        Current stack: [(Int 1); (Int 8)]
        Current env: []
      Executing:
        Current Instruction: IADD
        Current stack: [(Int 1); (Int 8)]
        Current env: []
      Executing:
        Current Instruction: (CONST 1)
        Current stack: [(Int 1); (Int 9)]
        Current env: []
      Executing:
        Current Instruction: IADD
        Current stack: [(Int 1); (Int 9)]
        Current env: []
      Executing:
        Current Instruction: (CONST 1)
        Current stack: [(Int 1); (Int 10)]
        Current env: []
      Executing:
        Current Instruction: IADD
        Current stack: [(Int 1); (Int 10)]
        Current env: []
      Executing:
        Current Instruction: (CONST 1)
        Current stack: [(Int 1); (Int 11)]
        Current env: []
      Executing:
        Current Instruction: IADD
        Current stack: [(Int 1); (Int 11)]
        Current env: []
      Executing:
        Current Instruction: RETURN
        Current stack: [(Int 1); (Int 12)]
        Current env: [] |}]

  let%expect_test "trace branching" =
    Vm.intepret
      ~debug:true
      (Vm.default ())
      ~remaining_gas:max_int
      ~code:code_branching
      ~stack:[||] ;
    [%expect
      {|
      Executing:
        Current Instruction: (CONST 0)
        Current stack: []
        Current env: []
      Executing:
        Current Instruction: (BRANCHIFNOT 8)
        Current stack: [(Int 0)]
        Current env: []
      Executing:
        Current Instruction: (CLOSURE 2)
        Current stack: [(Int 0)]
        Current env: []
      Executing:
        Current Instruction: (CONST 2)
        Current stack: [Closure {jmp_dest = 2; env_pointer = 0}]
        Current env: []
      Executing:
        Current Instruction: APPLY
        Current stack: [(Int 2); Closure {jmp_dest = 2; env_pointer = 0}]
        Current env: []
      Executing:
        Current Instruction: (ACCESS 1)
        Current stack: [(Int 2); Closure {jmp_dest = 13; env_pointer = 0}]
        Current env: [(Int 2)]
      Executing:
        Current Instruction: (CONST 0)
        Current stack: [(Int 2); Closure {jmp_dest = 13; env_pointer = 0}]
        Current env: [(Int 2)]
      Executing:
        Current Instruction: EQINT
        Current stack: [(Int 0); (Int 2); Closure {jmp_dest = 13; env_pointer = 0}]
        Current env: [(Int 2)]
      Executing:
        Current Instruction: (BRANCHIFNOT 2)
        Current stack: [(Int 0); (Int 0); Closure {jmp_dest = 13; env_pointer = 0}]
        Current env: [(Int 2)]
      Executing:
        Current Instruction: (CONST 0)
        Current stack: [(Int 0); Closure {jmp_dest = 13; env_pointer = 0}]
        Current env: [(Int 2)]
      Executing:
        Current Instruction: RETURN
        Current stack: [(Int 0); (Int 0); Closure {jmp_dest = 13; env_pointer = 0}]
        Current env: [(Int 2)]
      Executing:
        Current Instruction: RETURN
        Current stack: [(Int 0); (Int 0)]
        Current env: [(Int 2)] |}]

  let%expect_test "trace closures" =
    Vm.intepret
      ~debug:true
      (Vm.default ())
      ~remaining_gas:max_int
      ~code:code2
      ~stack:[||] ;
    [%expect
      {|
      Executing:
        Current Instruction: (CONST 0)
        Current stack: []
        Current env: []
      Executing:
        Current Instruction: (BRANCHIFNOT 4)
        Current stack: [(Int 0)]
        Current env: []
      Executing:
        Current Instruction: (CLOSURE 2)
        Current stack: [(Int 0)]
        Current env: []
      Executing:
        Current Instruction: (CONST 2)
        Current stack: [Closure {jmp_dest = 2; env_pointer = 0}]
        Current env: []
      Executing:
        Current Instruction: APPLY
        Current stack: [(Int 2); Closure {jmp_dest = 2; env_pointer = 0}]
        Current env: []
      Executing:
        Current Instruction: (ACCESS 1)
        Current stack: [(Int 2); Closure {jmp_dest = 9; env_pointer = 0}]
        Current env: [(Int 2)]
      Executing:
        Current Instruction: (CONST 1)
        Current stack: [(Int 2); Closure {jmp_dest = 9; env_pointer = 0}]
        Current env: [(Int 2)]
      Executing:
        Current Instruction: IADD
        Current stack: [(Int 1); (Int 2); Closure {jmp_dest = 9; env_pointer = 0}]
        Current env: [(Int 2)]
      Executing:
        Current Instruction: RETURN
        Current stack: [(Int 1); (Int 3); Closure {jmp_dest = 9; env_pointer = 0}]
        Current env: [(Int 2)]
      Executing:
        Current Instruction: RETURN
        Current stack: [(Int 3); (Int 3)]
        Current env: [(Int 2)] |}]

  let%expect_test "trace bytes" =
    Vm.intepret
      ~debug:true
      (Vm.default ())
      ~remaining_gas:max_int
      ~code:code_str
      ~stack:[||] ;
    [%expect
      {|
      Executing:
        Current Instruction: (CONSTBYTES "Test")
        Current stack: []
        Current env: []
      Executing:
        Current Instruction: (CONSTBYTES "Test")
        Current stack: [(Bytes "Test")]
        Current env: []
      Executing:
        Current Instruction: (CONSTBYTES "Test")
        Current stack: [(Bytes "Test"); (Bytes "Test")]
        Current env: []
      Executing:
        Current Instruction: (CONSTBYTES "Test")
        Current stack: [(Bytes "Test"); (Bytes "Test"); (Bytes "Test")]
        Current env: []
      Executing:
        Current Instruction: (CONSTBYTES "Test")
        Current stack: [(Bytes "Test"); (Bytes "Test"); (Bytes "Test"); (Bytes "Test")]
        Current env: []
      Executing:
        Current Instruction: RETURN
        Current stack: [(Bytes "Test"); (Bytes "Test"); (Bytes "Test"); (Bytes "Test");
        (Bytes "Test")]
        Current env: [] |}]
end
