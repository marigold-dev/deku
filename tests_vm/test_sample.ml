[@@@warning "-40"]
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

  let code_lst =
    Vm.
      [|
        CONST 0;
        CONST 1;
        CONST 2;
        CONST 1;
        MAKE_BLOCK;
        CONST 3;
        CONST 2;
        CONST 1;
        MAKE_BLOCK;
        RETURN;
        CONST 7;
        LET;
        (* env: 7 :: []*)
        CONST 9;
        LET;
        (* env: 9 :: 7 :: []*)
        CONST 2;
        MAKE_BLOCK;
        LET;
        (* env: ptr_cons_9 :: 9 :: 7 :: []*)
        ACCESS 0;
        ACCESS 1;
        WRITE_FIELD;
        ACCESS 0;
        CONST 1;
        IADD;
        CONST 0;
        WRITE_FIELD;
        CONST 2;
        MAKE_BLOCK;
        LET;
        (* env: ptr_cons_7 :: ptr_cons_9 :: 9 :: 7 :: []*)
        ACCESS 0;
        ACCESS 3;
        WRITE_FIELD;
        ACCESS 0;
        CONST 1;
        IADD;
        ACCESS 1;
        WRITE_FIELD;
        ACCESS 0;
        ENDLET;
        (* env: ptr_cons_9 :: 9 :: 7 :: [] *)
        ENDLET;
        (* env: 9 :: 7 :: [] *)
        ENDLET;
        (* env: 7 :: [] *)
        ENDLET (* env: [] *);
      |]

  let incr_contract =
    Vm.
      [|
        BRANCH 0;
        ACCESS 1;
        CONST 1;
        IADD;
        RETURN;
        ACCESS 1;
        CONST 1;
        ISUB;
        RETURN;
        CLOSURE 1;
        CLOSURE 6;
      |]

  type 'a ty = Int | Block of 'a ty list

  let read memory pointer = Array.get memory pointer

  let rec dump fmt memory pointer ty =
    let fprintf s = Format.fprintf fmt s in
    match ty with
    | Int ->
        let x = read memory pointer in
        fprintf "%d" x
    | Block tys ->
        let pointer = read memory pointer in
        fprintf "(" ;
        List.iteri
          (fun i ty ->
            if i <> 0 then fprintf ", " ;
            dump fmt memory (pointer + i) ty)
          tys ;
        fprintf ")"

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

  let%test "call 2" =
    let state = Vm.make_default () in
    let incr_contract =
      Vm.
        [|
          BRANCH 31;
          (* begin incr *)
          ACCESS 1;
          CONST 1;
          IADD;
          RETURN;
          (*  end incr *)
          (* begin decr *)
          ACCESS 1;
          CONST 1;
          ISUB;
          RETURN;
          (* end  decr *)
          (* main fn *)
          ACCESS 1;
          CONST 3;
          EQINT;
          BRANCHIFNOT 2;
          (* case of RESET *)
          CONST 0;
          RETURN;
          (* CASE OF DECR *)
          ACCESS 1;
          CONST 2;
          EQINT;
          BRANCHIFNOT 5;
          ACCESS 2;
          CLOSURE 5;
          SWAP;
          APPLY;
          RETURN;
          (* CASE oF INCR *)
          ACCESS 2;
          CLOSURE 1;
          SWAP;
          APPLY;
          RETURN;
          CLOSURE 9;
          RETURN;
          CLOSURE 29;
          SWAP;
          APPLY;
          SWAP;
          APPLY;
          RETURN;
        |]
    in

    Vm.intepret
      state
      ~remaining_gas:max_int
      ~debug:false
      ~code:incr_contract
      ~stack:[|3; 1|] ;
    let res = Vm.Stack.pop state.stack in
    res = 0

  (* let%expect_test "trace addition" =
       Vm.intepret
         ~debug:true
         (Vm.make_default ())
         ~remaining_gas:max_int
         ~code
         ~stack:[||] ;
       [%expect
         {|
         Executing:
           Current Instruction: (CONST 1)
           Current stack: []
           Current env: []
           Remaining Gas: 4611686018427387902
         Executing:
           Current Instruction: (CONST 1)
           Current stack: [(Int 1)]
           Current env: []
           Remaining Gas: 4611686018427387901
         Executing:
           Current Instruction: IADD
           Current stack: [(Int 1); (Int 1)]
           Current env: []
           Remaining Gas: 4611686018427387900
         Executing:
           Current Instruction: (CONST 1)
           Current stack: [(Int 2)]
           Current env: []
           Remaining Gas: 4611686018427387899
         Executing:
           Current Instruction: IADD
           Current stack: [(Int 1); (Int 2)]
           Current env: []
           Remaining Gas: 4611686018427387898
         Executing:
           Current Instruction: (CONST 1)
           Current stack: [(Int 3)]
           Current env: []
           Remaining Gas: 4611686018427387897
         Executing:
           Current Instruction: IADD
           Current stack: [(Int 1); (Int 3)]
           Current env: []
           Remaining Gas: 4611686018427387896
         Executing:
           Current Instruction: (CONST 1)
           Current stack: [(Int 4)]
           Current env: []
           Remaining Gas: 4611686018427387895
         Executing:
           Current Instruction: IADD
           Current stack: [(Int 1); (Int 4)]
           Current env: []
           Remaining Gas: 4611686018427387894
         Executing:
           Current Instruction: (CONST 1)
           Current stack: [(Int 5)]
           Current env: []
           Remaining Gas: 4611686018427387893
         Executing:
           Current Instruction: IADD
           Current stack: [(Int 1); (Int 5)]
           Current env: []
           Remaining Gas: 4611686018427387892
         Executing:
           Current Instruction: (CONST 1)
           Current stack: [(Int 6)]
           Current env: []
           Remaining Gas: 4611686018427387891
         Executing:
           Current Instruction: IADD
           Current stack: [(Int 1); (Int 6)]
           Current env: []
           Remaining Gas: 4611686018427387890
         Executing:
           Current Instruction: (CONST 1)
           Current stack: [(Int 7)]
           Current env: []
           Remaining Gas: 4611686018427387889
         Executing:
           Current Instruction: IADD
           Current stack: [(Int 1); (Int 7)]
           Current env: []
           Remaining Gas: 4611686018427387888
         Executing:
           Current Instruction: (CONST 1)
           Current stack: [(Int 8)]
           Current env: []
           Remaining Gas: 4611686018427387887
         Executing:
           Current Instruction: IADD
           Current stack: [(Int 1); (Int 8)]
           Current env: []
           Remaining Gas: 4611686018427387886
         Executing:
           Current Instruction: (CONST 1)
           Current stack: [(Int 9)]
           Current env: []
           Remaining Gas: 4611686018427387885
         Executing:
           Current Instruction: IADD
           Current stack: [(Int 1); (Int 9)]
           Current env: []
           Remaining Gas: 4611686018427387884
         Executing:
           Current Instruction: (CONST 1)
           Current stack: [(Int 10)]
           Current env: []
           Remaining Gas: 4611686018427387883
         Executing:
           Current Instruction: IADD
           Current stack: [(Int 1); (Int 10)]
           Current env: []
           Remaining Gas: 4611686018427387882
         Executing:
           Current Instruction: (CONST 1)
           Current stack: [(Int 11)]
           Current env: []
           Remaining Gas: 4611686018427387881
         Executing:
           Current Instruction: IADD
           Current stack: [(Int 1); (Int 11)]
           Current env: []
           Remaining Gas: 4611686018427387880
         Executing:
           Current Instruction: RETURN
           Current stack: [(Int 12)]
           Current env: []
           Remaining Gas: 4611686018427387879 |}]

     let%expect_test "trace branching" =
       Vm.intepret
         ~debug:true
         (Vm.make_default ())
         ~remaining_gas:max_int
         ~code:code_branching
         ~stack:[||] ;
       [%expect
         {|
         Executing:
           Current Instruction: (CONST 0)
           Current stack: []
           Current env: []
           Remaining Gas: 4611686018427387902
         Executing:
           Current Instruction: (BRANCHIFNOT 8)
           Current stack: [(Int 0)]
           Current env: []
           Remaining Gas: 4611686018427387901
         Executing:
           Current Instruction: (CLOSURE 2)
           Current stack: []
           Current env: []
           Remaining Gas: 4611686018427387900
         Executing:
           Current Instruction: (CONST 2)
           Current stack: [Closure {jmp_dest = 2; env_pointer = 0}]
           Current env: []
           Remaining Gas: 4611686018427387899
         Executing:
           Current Instruction: APPLY
           Current stack: [(Int 2); Closure {jmp_dest = 2; env_pointer = 0}]
           Current env: []
           Remaining Gas: 4611686018427387898
         Executing:
           Current Instruction: (ACCESS 1)
           Current stack: [Closure {jmp_dest = 13; env_pointer = 0}]
           Current env: [(Int 2)]
           Remaining Gas: 4611686018427387897
         Executing:
           Current Instruction: (CONST 0)
           Current stack: [(Int 2); Closure {jmp_dest = 13; env_pointer = 0}]
           Current env: [(Int 2)]
           Remaining Gas: 4611686018427387896
         Executing:
           Current Instruction: EQINT
           Current stack: [(Int 0); (Int 2); Closure {jmp_dest = 13; env_pointer = 0}]
           Current env: [(Int 2)]
           Remaining Gas: 4611686018427387895
         Executing:
           Current Instruction: (BRANCHIFNOT 2)
           Current stack: [(Int 0); Closure {jmp_dest = 13; env_pointer = 0}]
           Current env: [(Int 2)]
           Remaining Gas: 4611686018427387894
         Executing:
           Current Instruction: (CONST 0)
           Current stack: [Closure {jmp_dest = 13; env_pointer = 0}]
           Current env: [(Int 2)]
           Remaining Gas: 4611686018427387893
         Executing:
           Current Instruction: RETURN
           Current stack: [(Int 0); Closure {jmp_dest = 13; env_pointer = 0}]
           Current env: [(Int 2)]
           Remaining Gas: 4611686018427387892
         Executing:
           Current Instruction: RETURN
           Current stack: [(Int 0)]
           Current env: []
           Remaining Gas: 4611686018427387891 |}]

     let%expect_test "trace closures" =
       Vm.intepret
         ~debug:true
         (Vm.make_default ())
         ~remaining_gas:max_int
         ~code:code2
         ~stack:[||] ;
       [%expect
         {|
         Executing:
           Current Instruction: (CONST 0)
           Current stack: []
           Current env: []
           Remaining Gas: 4611686018427387902
         Executing:
           Current Instruction: (BRANCHIFNOT 4)
           Current stack: [(Int 0)]
           Current env: []
           Remaining Gas: 4611686018427387901
         Executing:
           Current Instruction: (CLOSURE 2)
           Current stack: []
           Current env: []
           Remaining Gas: 4611686018427387900
         Executing:
           Current Instruction: (CONST 2)
           Current stack: [Closure {jmp_dest = 2; env_pointer = 0}]
           Current env: []
           Remaining Gas: 4611686018427387899
         Executing:
           Current Instruction: APPLY
           Current stack: [(Int 2); Closure {jmp_dest = 2; env_pointer = 0}]
           Current env: []
           Remaining Gas: 4611686018427387898
         Executing:
           Current Instruction: (ACCESS 1)
           Current stack: [Closure {jmp_dest = 9; env_pointer = 0}]
           Current env: [(Int 2)]
           Remaining Gas: 4611686018427387897
         Executing:
           Current Instruction: (CONST 1)
           Current stack: [(Int 2); Closure {jmp_dest = 9; env_pointer = 0}]
           Current env: [(Int 2)]
           Remaining Gas: 4611686018427387896
         Executing:
           Current Instruction: IADD
           Current stack: [(Int 1); (Int 2); Closure {jmp_dest = 9; env_pointer = 0}]
           Current env: [(Int 2)]
           Remaining Gas: 4611686018427387895
         Executing:
           Current Instruction: RETURN
           Current stack: [(Int 3); Closure {jmp_dest = 9; env_pointer = 0}]
           Current env: [(Int 2)]
           Remaining Gas: 4611686018427387894
         Executing:
           Current Instruction: RETURN
           Current stack: [(Int 3)]
           Current env: []
           Remaining Gas: 4611686018427387893 |}]

     let%expect_test "trace bytes" =
       Vm.intepret
         ~debug:true
         (Vm.make_default ())
         ~remaining_gas:max_int
         ~code:code_str
         ~stack:[||] ;
       [%expect
         {|
         Executing:
           Current Instruction: (CONSTBYTES "Test")
           Current stack: []
           Current env: []
           Remaining Gas: 4611686018427387902
         Executing:
           Current Instruction: (CONSTBYTES "Test")
           Current stack: [(Bytes "Test")]
           Current env: []
           Remaining Gas: 4611686018427387897
         Executing:
           Current Instruction: (CONSTBYTES "Test")
           Current stack: [(Bytes "Test"); (Bytes "Test")]
           Current env: []
           Remaining Gas: 4611686018427387892
         Executing:
           Current Instruction: (CONSTBYTES "Test")
           Current stack: [(Bytes "Test"); (Bytes "Test"); (Bytes "Test")]
           Current env: []
           Remaining Gas: 4611686018427387887
         Executing:
           Current Instruction: (CONSTBYTES "Test")
           Current stack: [(Bytes "Test"); (Bytes "Test"); (Bytes "Test"); (Bytes "Test")]
           Current env: []
           Remaining Gas: 4611686018427387882
         Executing:
           Current Instruction: RETURN
           Current stack: [(Bytes "Test"); (Bytes "Test"); (Bytes "Test"); (Bytes "Test");
           (Bytes "Test")]
           Current env: []
           Remaining Gas: 4611686018427387877 |}]

     let%expect_test "trace addition" =
       Vm.intepret
         ~debug:true
         (Vm.make_default ())
         ~remaining_gas:max_int
         ~code:code_lst
         ~stack:[||] ;
       [%expect
         {|
         Executing:
           Current Instruction: (CONST 0)
           Current stack: []
           Current env: []
           Remaining Gas: 4611686018427387902
         Executing:
           Current Instruction: (CONST 1)
           Current stack: [(Int 0)]
           Current env: []
           Remaining Gas: 4611686018427387901
         Executing:
           Current Instruction: (CONST 2)
           Current stack: [(Int 1); (Int 0)]
           Current env: []
           Remaining Gas: 4611686018427387900
         Executing:
           Current Instruction: (CONST 1)
           Current stack: [(Int 2); (Int 1); (Int 0)]
           Current env: []
           Remaining Gas: 4611686018427387899
         Executing:
           Current Instruction: MAKE_BLOCK
           Current stack: [(Int 1); (Int 2); (Int 1); (Int 0)]
           Current env: []
           Remaining Gas: 4611686018427387898
         Executing:
           Current Instruction: (CONST 3)
           Current stack: [(List [(Int 1)])]
           Current env: []
           Remaining Gas: 4611686018427387895
         Executing:
           Current Instruction: (CONST 2)
           Current stack: [(Int 3); (List [(Int 1)])]
           Current env: []
           Remaining Gas: 4611686018427387894
         Executing:
           Current Instruction: (CONST 1)
           Current stack: [(Int 2); (Int 3); (List [(Int 1)])]
           Current env: []
           Remaining Gas: 4611686018427387893
         Executing:
           Current Instruction: MAKE_BLOCK
           Current stack: [(Int 1); (Int 2); (Int 3); (List [(Int 1)])]
           Current env: []
           Remaining Gas: 4611686018427387892
         Executing:
           Current Instruction: RETURN
           Current stack: [(List [(Int 3); (Int 1)])]
           Current env: []
           Remaining Gas: 4611686018427387889 |}] *)
end
