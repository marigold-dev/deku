(*

  All of these tests are copied from here:
  https://github.com/marigold-dev/deku/blob/smart-contracts/ligolang/src/test/zinc_tests.ml

  If you want to add tests, you should add them to that branch too, so you can test the compiler
  at the same time.

*)

open Zinc_utils
module Zinc_interpreter = Zinc_interpreter.Dummy
open Zinc_interpreter.Types
open Zinc_interpreter
open Zinc
open Types.Stack_item

let test_w name t = Alcotest.test_case name `Quick t

type dialect = ReasonLIGO | CameLIGO | JsLIGO | PascaLIGO

(* Use `dune build -w @zinctest --no-buffer` to run just the zinc tests! *)
module Executor : Executor = struct
  let get_contract_opt a = Some (a, None)

  let chain_id = "chain id goes here"

  let hash = Fun.id

  let key_hash s = s ^ "hash"
end
(* Helpers *)

(* Alcotest setup *)

let expect_program =
  Alcotest.(
    check
      (Alcotest.testable
         (fun ppf program -> Fmt.pf ppf "%s" (Program.to_string program))
         Program.equal))

let expect_code =
  Alcotest.(
    check
      (Alcotest.testable
         (fun ppf zinc -> Fmt.pf ppf "%s" (Zinc.to_string zinc))
         Zinc.equal))

let expect_env =
  Alcotest.(
    check
      (Alcotest.testable
         (fun ppf env -> Fmt.pf ppf "%s" (Types.Env.to_string env))
         Types.Env.equal))

let expect_stack =
  Alcotest.(
    check
      (Alcotest.testable
         (fun ppf stack -> Fmt.pf ppf "%s" (Types.Stack.to_string stack))
         Types.Stack.equal))

type test = unit -> unit

let expect_simple_compile_to ?dialect:_ ?index ?(initial_stack = [])
    ?expect_failure ?expected_output_env ?expected_output ?expected_json
    contract_file (zinc : Program.t) : test =
 fun () ->
  (*
  let to_zinc = to_zinc ~raise ~add_warning in
  let ext =
    match dialect with
    | CameLIGO -> "mligo"
    | PascaLIGO -> "ligo"
    | ReasonLIGO -> "religo"
    | JsLIGO -> "jsligo"
  in
  let contract = Printf.sprintf "./contracts/%s.%s" contract_file ext in
  let zinc = to_zinc contract in
  let () =
    expect_program
      (Printf.sprintf "compiling %s" contract_file)
      expected_zinc zinc
  in
  *)
  let () =
    match expected_json with
    | Some expected_json ->
        Alcotest.(check string)
          (Printf.sprintf "converting %s to json" contract_file)
          expected_json
          (Program.to_yojson zinc |> Yojson.Safe.to_string)
    | _ -> ()
  in
  let index = match index with None -> List.length zinc - 1 | Some n -> n in
  match
    ( expect_failure,
      let from = Base.List.nth_exn zinc index |> snd |> Zinc.to_yojson in
      let to_ = Types.Zinc.of_yojson from |> Result.get_ok in
      to_
      |> Interpreter.initial_state ~initial_stack
      |> Interpreter.eval (module Executor) )
  with
  | (None, Interpreter_output.Success (output_env, output_stack)) ->
      let () =
        match expected_output_env with
        | Some expected_zinc ->
            expect_env
              (Printf.sprintf "evaluating env for %s" contract_file)
              expected_zinc
              output_env
        | None -> ()
      in
      let () =
        match expected_output with
        | Some expected_stack ->
            expect_stack
              (Printf.sprintf "evaluating stack for %s" contract_file)
              expected_stack
              output_stack
        | None -> ()
      in
      ()
  | (Some s, Interpreter_output.Failure s') ->
      Alcotest.(check string) "hmm" s s'
  | (Some _, Interpreter_output.Success _) ->
      failwith "expected failure, but execution was successful"
  | (None, Interpreter_output.Failure _) ->
      failwith "was not expecting failure, but execution failed anyway"

(* ================ *)
(* Tests *)

let simple_1 =
  expect_simple_compile_to
    "simple1"
    [("i", [Plain_old_data (Num (Z.of_int 42)); Core Return])]
    ~expected_output:[Types.Stack_item.Z (Plain_old_data (Num (Z.of_int 42)))]

let simple_2 =
  expect_simple_compile_to
    "simple2"
    [("i", [Plain_old_data (Num (Z.of_int 42)); Core Return])]
    ~expected_output:[Types.Stack_item.Z (Plain_old_data (Num (Z.of_int 42)))]

let simple_3 =
  expect_simple_compile_to
    "simple3"
    [
      ( "my_address",
        [
          Plain_old_data (Address "tz1KqTpEZ7Yob7QbPE4Hy4Wo8fHG8LhKxZSx");
          Core Return;
        ] );
    ]
    ~expected_output:
      [
        Types.Stack_item.Z
          (Plain_old_data (Address "tz1KqTpEZ7Yob7QbPE4Hy4Wo8fHG8LhKxZSx"));
      ]

let id =
  expect_simple_compile_to
    "id_func"
    [("id", [Core Grab; Core (Access 0); Core Return])]
    ~initial_stack:[Types.Stack_item.Z (Plain_old_data (Num (Z.of_int 42)))]
    ~expected_output:[Types.Stack_item.Z (Plain_old_data (Num (Z.of_int 42)))]

let chain_id =
  expect_simple_compile_to
    "chain_id"
    [("chain_id", [Domain_specific_operation ChainID; Core Return])]
    ~expected_output:[Z (Plain_old_data (Chain_id "chain id goes here"))]

let chain_id_func =
  expect_simple_compile_to
    "chain_id_func"
    [("chain_id", [Core Grab; Domain_specific_operation ChainID; Core Return])]
    ~initial_stack:[Types.Utils.unit_record_stack]

let tuple_creation =
  expect_simple_compile_to
    "tuple_creation"
    [
      ( "dup",
        [
          Core Grab;
          Core (Access 0);
          Core (Access 0);
          Adt (MakeRecord 2);
          Core Return;
        ] );
    ]
    ~initial_stack:[Types.Stack_item.Z (Plain_old_data (Num Z.one))]
    ~expected_output:
      [
        Types.Stack_item.Record
          Zinc_utils.LMap.(
            let one = Types.Stack_item.Z (Plain_old_data (Num Z.one)) in
            empty |> add 0 one |> add 1 one);
      ]

let check_record_destructure =
  expect_simple_compile_to
    "check_record_destructure"
    [
      ( "check_record_destructure",
        [
          Core Grab;
          Core (Access 0);
          Core Grab;
          Core (Access 0);
          Core Grab;
          Core (Access 0);
          Adt (RecordAccess 1);
          Core Grab;
          Core (Access 1);
          Adt (RecordAccess 0);
          Core Grab;
          Core (Access 1);
          Core (Access 0);
          Operation Add;
          Core Return;
        ] );
    ]
    ~initial_stack:
      [
        Types.Stack_item.Record
          Zinc_utils.LMap.(
            let one = Types.Stack_item.Z (Plain_old_data (Num Z.one)) in
            empty |> add 0 one |> add 1 one);
      ]

let check_hash_key =
  let open Zinc_utils in
  expect_simple_compile_to
    "key_hash"
    [
      ( "check_hash_key",
        [
          Core Grab;
          Core (Access 0);
          Core Grab;
          Core (Access 0);
          Core Grab;
          Core (Access 0);
          Adt (RecordAccess 1);
          Core Grab;
          Core (Access 1);
          Adt (RecordAccess 0);
          Core Grab;
          Core (Access 1);
          Operation HashKey;
          Core Grab;
          Core (Access 0);
          Core (Access 0);
          Core (Access 1);
          Operation Eq;
          Adt (MakeRecord 2);
          Core Return;
        ] );
    ]
    ~initial_stack:
      [
        Types.Stack_item.Record
          (LMap.empty
          |> LMap.add
               0
               (Types.Stack_item.Z (Plain_old_data (Hash "not sure yet")))
          |> LMap.add
               1
               (Types.Stack_item.Z (Plain_old_data (Key "Hashy hash!"))));
      ]

let basic_function_application =
  expect_simple_compile_to
    ~dialect:ReasonLIGO
    "basic_function_application"
    [
      ( "a",
        [
          Plain_old_data (Num (Z.of_int 3));
          Core Grab;
          Core (Access 0);
          Core Return;
        ] );
    ]
    ~expected_output:[Types.Stack_item.Z (Plain_old_data (Num (Z.of_int 3)))]

let basic_link =
  expect_simple_compile_to
    ~dialect:ReasonLIGO
    "basic_link"
    [
      ("a", [Plain_old_data (Num (Z.of_int 1)); Core Return]);
      ( "b",
        [
          Plain_old_data (Num (Z.of_int 1));
          Core Grab;
          Core (Access 0);
          Core Return;
        ] );
    ]
    ~index:1
    ~expected_output:[Types.Stack_item.Z (Plain_old_data (Num (Z.of_int 1)))]

let failwith_simple =
  expect_simple_compile_to
    ~dialect:ReasonLIGO
    "failwith_simple"
    [
      ( "a",
        [
          Plain_old_data (String "Not a contract");
          Control_flow Failwith;
          Core Return;
        ] );
    ]
    ~expect_failure:"Not a contract"

let get_contract_opt =
  expect_simple_compile_to
    ~dialect:ReasonLIGO
    "get_contract_opt"
    [
      ( "a",
        [
          Plain_old_data (Address "whatever");
          Domain_specific_operation Contract_opt;
          Core Return;
        ] );
    ]
    ~expected_output:
      [
        Types.Stack_item.Variant
          (0, Types.Stack_item.NonliteralValue (Contract ("whatever", None)));
      ]

let match_on_sum =
  expect_simple_compile_to
    ~dialect:ReasonLIGO
    "match_on_sum"
    [
      ( "a",
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
                 [Core Grab; Core (Access 0); Core Return];
                 [
                   Core Grab;
                   Plain_old_data (String "Not a contract");
                   Control_flow Failwith;
                   Core Return;
                 ];
               |]);
        ] );
    ]
    ~expected_output:
      [
        Types.Stack_item.NonliteralValue
          (Contract ("tz1TGu6TN5GSez2ndXXeDX6LgUDvLzPLqgYV", None));
      ]

let super_simple_contract =
  let open Z in
  expect_simple_compile_to
    ~dialect:ReasonLIGO
    "super_simple_contract"
    [
      ( "main",
        [
          Core Grab;
          Core (Access 0);
          Core Grab;
          Core (Access 0);
          Core Grab;
          Core (Access 0);
          Adt (RecordAccess 1);
          Core Grab;
          Core (Access 1);
          Adt (RecordAccess 0);
          Core Grab;
          Plain_old_data (Num ~$1);
          Core (Access 1);
          Operation Add;
          Plain_old_data Nil;
          Adt (MakeRecord 2);
          Core Return;
        ] );
    ]
    ~initial_stack:
      [
        Types.Stack_item.Record
          [|
            Types.Utils.unit_record_stack;
            Types.Stack_item.Z (Plain_old_data (Num ~$1));
          |];
      ]
    ~expected_output:
      [
        Types.Stack_item.Record
          [|
            Types.Stack_item.List [];
            Types.Stack_item.Z (Plain_old_data (Num ~$2));
          |];
      ]
    ~expected_json:
      "[[\"main\",[[\"Core\",[\"Grab\"]],[\"Core\",[\"Access\",0]],[\"Core\",[\"Grab\"]],[\"Core\",[\"Access\",0]],[\"Core\",[\"Grab\"]],[\"Core\",[\"Access\",0]],[\"Adt\",[\"RecordAccess\",1]],[\"Core\",[\"Grab\"]],[\"Core\",[\"Access\",1]],[\"Adt\",[\"RecordAccess\",0]],[\"Core\",[\"Grab\"]],[\"Plain_old_data\",[\"Num\",\"1\"]],[\"Core\",[\"Access\",1]],[\"Operation\",[\"Add\"]],[\"Plain_old_data\",[\"Nil\"]],[\"Adt\",[\"MakeRecord\",2]],[\"Core\",[\"Return\"]]]]]"
(* below this line are tests that fail because I haven't yet implemented the necessary primatives *)

let mutez_construction =
  expect_simple_compile_to
    ~dialect:ReasonLIGO
    "mutez_construction"
    [("a", [Plain_old_data (Mutez (Z.of_int 1)); Core Return])]

let nontail_match =
  let open Z in
  expect_simple_compile_to
    ~dialect:ReasonLIGO
    "nontail_match"
    [
      ( "a",
        [
          Plain_old_data (Num ~$4);
          Core Grab;
          Plain_old_data (Num ~$3);
          Adt (MakeVariant 0);
          Core Grab;
          Core (Access 0);
          Core Grab;
          Core (Access 0);
          Adt
            (MatchVariant
               [|
                 [
                   Core Grab;
                   Plain_old_data (Num ~$5);
                   Core (Access 0);
                   Operation Add;
                   Core EndLet;
                 ];
                 [
                   Core Grab;
                   Plain_old_data (String "should be some >:(");
                   Control_flow Failwith;
                   Core EndLet;
                 ];
               |]);
          Core EndLet;
          Core Grab;
          Core (Access 2);
          Core (Access 0);
          Operation Add;
          Core Return;
        ] );
    ]
    ~expected_output:[Types.Stack_item.Z (Plain_old_data (Num ~$12))]

let create_transaction =
  let open Z in
  expect_simple_compile_to
    ~dialect:ReasonLIGO
    "create_transaction"
    [
      ( "a",
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
                 [Core Grab; Core (Access 0); Core EndLet];
                 [
                   Core Grab;
                   Plain_old_data (String "Not a contract");
                   Control_flow Failwith;
                   Core EndLet;
                 ];
               |]);
          Core EndLet;
          Core Grab;
          Core (Access 0);
          Plain_old_data (Mutez ~$10);
          Adt (MakeRecord 0);
          Domain_specific_operation MakeTransaction;
          Core Return;
        ] );
    ]
    ~expected_output:
      [
        Types.Stack_item.NonliteralValue
          (Chain_operation
             (Transaction
                (Z.of_int 10, ("tz1TGu6TN5GSez2ndXXeDX6LgUDvLzPLqgYV", None))));
      ]

let create_transaction_in_tuple =
  let open Z in
  expect_simple_compile_to
    ~dialect:ReasonLIGO
    "create_transaction_in_tuple"
    [
      ( "a",
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
                 [Core Grab; Core (Access 0); Core EndLet];
                 [
                   Core Grab;
                   Plain_old_data (String "Not a contract");
                   Control_flow Failwith;
                   Core EndLet;
                 ];
               |]);
          Core EndLet;
          Core Grab;
          Plain_old_data (String "my string");
          Plain_old_data
            (Key "edpkuBknW28nW72KG6RoHtYW7p12T6GKc7nAbwYX5m8Wd9sDVC9yav");
          Core (Access 0);
          Plain_old_data (Mutez ~$10);
          Adt (MakeRecord 0);
          Domain_specific_operation MakeTransaction;
          Adt (MakeRecord 3);
          Core Return;
        ] );
    ]
    ~expected_output:
      [
        Types.Stack_item.Record
          LMap.(
            empty
            |> add
                 0
                 (Types.Stack_item.NonliteralValue
                    (Chain_operation
                       (Transaction
                          ( Z.of_int 10,
                            ("tz1TGu6TN5GSez2ndXXeDX6LgUDvLzPLqgYV", None) ))))
            |> add
                 1
                 (Types.Stack_item.Z
                    (Plain_old_data
                       (Key
                          "edpkuBknW28nW72KG6RoHtYW7p12T6GKc7nAbwYX5m8Wd9sDVC9yav")))
            |> add 2 (Types.Stack_item.Z (Plain_old_data (String "my string"))));
      ]

let list_construction =
  expect_simple_compile_to
    ~dialect:ReasonLIGO
    "list_construction"
    [
      ( "a",
        [
          Plain_old_data Nil;
          Plain_old_data (Mutez (Z.of_int 2));
          Operation Cons;
          Plain_old_data (Mutez (Z.of_int 1));
          Operation Cons;
          Core Return;
        ] );
    ]
    ~expected_output:
      [
        Types.Stack_item.List
          [
            Types.Stack_item.Z (Plain_old_data (Mutez (Z.of_int 1)));
            Types.Stack_item.Z (Plain_old_data (Mutez (Z.of_int 2)));
          ];
      ]

let bools_religo =
  expect_simple_compile_to
    ~dialect:ReasonLIGO
    "bools"
    [
      ("a", [Plain_old_data (Bool true); Core Return]);
      ( "b",
        [
          Plain_old_data (Bool true);
          Core Grab;
          Plain_old_data (Bool false);
          Core Return;
        ] );
      ( "lf",
        [
          Plain_old_data (Bool true);
          Core Grab;
          Plain_old_data (Bool false);
          Core Grab;
          Plain_old_data (Bool false);
          Core Grab;
          Core (Access 0);
          Core Grab;
          Core (Access 0);
          Adt
            (MatchVariant
               [|
                 [Core Grab; Core (Access 3); Core Return];
                 [Core Grab; Core (Access 4); Core Return];
               |]);
        ] );
    ]
    ~expected_output:[Types.Stack_item.Z (Plain_old_data (Bool false))]

let bools_ligo =
  expect_simple_compile_to
    ~dialect:PascaLIGO
    "bools"
    [
      ("a", [Plain_old_data (Bool true); Core Return]);
      ( "b",
        [
          Plain_old_data (Bool true);
          Core Grab;
          Plain_old_data (Bool false);
          Core Return;
        ] );
      ( "lf",
        [
          Plain_old_data (Bool true);
          Core Grab;
          Plain_old_data (Bool false);
          Core Grab;
          Plain_old_data (Bool false);
          Core Grab;
          Core (Access 0);
          Core Grab;
          Core (Access 0);
          Adt
            (MatchVariant
               [|
                 [Core Grab; Core (Access 3); Core Return];
                 [Core Grab; Core (Access 4); Core Return];
               |]);
        ] );
    ]
    ~expected_output:[Types.Stack_item.Z (Plain_old_data (Bool false))]

let bool_ops =
  expect_simple_compile_to
    ~dialect:PascaLIGO
    "boolean_operators"
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
    ~initial_stack:[Types.Stack_item.Z (Plain_old_data (Bool false))]
    ~expected_output:[Types.Stack_item.Z (Plain_old_data (Bool true))]

let if_then_else =
  let open Z in
  expect_simple_compile_to
    ~dialect:ReasonLIGO
    "if_then_else"
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
          Core Grab;
          Core (Access 0);
          Adt
            (MatchVariant
               [|
                 [Core Grab; Core (Access 2); Core Return];
                 [
                   Core Grab;
                   Plain_old_data (Num ~$2);
                   Core (Access 3);
                   Operation Add;
                   Core Return;
                 ];
               |]);
        ] );
    ]
    ~expected_output:[Types.Stack_item.Z (Plain_old_data (Num ~$4))]

let if_then_else_op =
  let open Z in
  expect_simple_compile_to
    ~dialect:PascaLIGO
    "if_then_else_op"
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
    ~expected_output:[Types.Stack_item.Z (Plain_old_data (Num ~$2))]

let if_then_else_op_function =
  let open Z in
  expect_simple_compile_to
    ~dialect:PascaLIGO
    "if_then_else_op_function"
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
          Core Grab;
          Core (Access 0);
          Core Grab;
          Core (Access 0);
          Core Grab;
          Core (Access 0);
          Adt (RecordAccess 1);
          Core Grab;
          Core (Access 1);
          Adt (RecordAccess 0);
          Core Grab;
          Core (Access 1);
          Core (Access 0);
          Operation Or;
          Core Grab;
          Core (Access 0);
          Adt
            (MatchVariant
               [|
                 [Core Grab; Core (Access 7); Core Return];
                 [Core Grab; Core (Access 8); Core Return];
               |]);
        ] );
    ]
    ~initial_stack:
      [
        Types.Stack_item.Record
          (LMap.empty
          |> LMap.add 0 (Types.Stack_item.Z (Plain_old_data (Bool false)))
          |> LMap.add 1 (Types.Stack_item.Z (Plain_old_data (Bool true))));
      ]
    ~expected_output:[Types.Stack_item.Z (Plain_old_data (Num ~$2))]

let make_an_option =
  expect_simple_compile_to
    ~dialect:ReasonLIGO
    "make_an_option"
    [
      ("a", [Adt (MakeRecord 0); Adt (MakeVariant 1); Core Return]);
      ( "b",
        [
          Adt (MakeRecord 0);
          (* constructing None, from the definition of a *)
          Adt (MakeVariant 1);
          Core Grab;
          Adt (MakeRecord 0);
          (* constructing Some *)
          Adt (MakeVariant 0);
          Core Return;
        ] );
    ]

let make_a_custom_option =
  expect_simple_compile_to
    ~dialect:ReasonLIGO
    "make_a_custom_option"
    [
      ("a", [Adt (MakeRecord 0); Adt (MakeVariant 0); Core Return]);
      ( "b",
        [
          Adt (MakeRecord 0);
          (* constructing My_none, from the definition of a *)
          Adt (MakeVariant 0);
          Core Grab;
          Adt (MakeRecord 0);
          (* constructing Some *)
          Adt (MakeVariant 1);
          Core Return;
        ] );
    ]

let top_level_let_dependencies =
  expect_simple_compile_to
    ~dialect:ReasonLIGO
    "top_level_let_dependencies"
    [
      ("a", [Plain_old_data (Num Z.one); Core Return]);
      ( "b",
        [Plain_old_data (Num Z.one); Core Grab; Core (Access 0); Core Return] );
      ( "c",
        [
          Plain_old_data (Num Z.one);
          Core Grab;
          Core (Access 0);
          Core Grab;
          Plain_old_data (Num Z.one);
          Core Return;
        ] );
    ]

let custom_variant_matching =
  let open Z in
  expect_simple_compile_to
    ~dialect:CameLIGO
    "custom_variant_matching"
    [
      ("a", [Plain_old_data (String "test"); Adt (MakeVariant 2); Core Return]);
      ( "b",
        [
          Plain_old_data (String "test");
          Adt (MakeVariant 2);
          Core Grab;
          Adt (MakeRecord 0);
          Adt (MakeVariant 0);
          Core Return;
        ] );
      ( "c",
        [
          Plain_old_data (String "test");
          Adt (MakeVariant 2);
          Core Grab;
          Adt (MakeRecord 0);
          Adt (MakeVariant 0);
          Core Grab;
          Core Grab;
          Core (Access 1);
          Core Grab;
          Core (Access 0);
          Adt
            (MatchVariant
               [|
                 [Core Grab; Plain_old_data (Num ~$1); Core Return];
                 [Core Grab; Plain_old_data (Num ~$2); Core Return];
                 [Core Grab; Plain_old_data (Num ~$0); Core Return];
               |]);
        ] );
    ]
    ~expected_output:[Z (Plain_old_data (Num ~$1))]
    ~initial_stack:[Types.Stack_item.Z (Types.Zinc.Plain_old_data (Bool false))]

let main =
  let open Alcotest in
  run
    "Zinc tests"
    [
      ( "string-case",
        [
          test_w "simple1" simple_1;
          test_w "simple2" simple_2;
          test_w "simple3" simple_3;
          test_w "bools_religo" bools_religo;
          test_w "bools_ligo" bools_ligo;
          test_w "bool_ops" bool_ops;
          test_w "if_then_else" if_then_else;
          test_w "if with ops" if_then_else_op;
          test_w "if with ops function" if_then_else_op_function;
          test_w "id" id;
          test_w "chain_id" chain_id;
          test_w "chain_id_func" chain_id_func;
          test_w "tuple_creation" tuple_creation;
          test_w "check_record_destructure" check_record_destructure;
          test_w "check_hash_key" check_hash_key;
          test_w "basic_function_application" basic_function_application;
          test_w "basic_link" basic_link;
          test_w "failwith_simple" failwith_simple;
          test_w "get_contract_opt" get_contract_opt;
          test_w "match_on_sum" match_on_sum;
          test_w "create_transaction" create_transaction;
          test_w "create_transaction_in_tuple" create_transaction_in_tuple;
          test_w "mutez_construction" mutez_construction;
          test_w "list_construction" list_construction;
          test_w "make_an_option" make_an_option;
          test_w "make_a_custom_option" make_a_custom_option;
          test_w "top_level_let_dependencies" top_level_let_dependencies;
          test_w "nontail_match" nontail_match;
          test_w "super_simple_contract" super_simple_contract;
          test_w "custom_variant_matching" custom_variant_matching;
        ] );
    ]
