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
      [
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

  let%expect_test "trace addition" =
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
        Remaining Gas: 4611686018427387889 |}]
end
