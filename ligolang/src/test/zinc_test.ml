open Zinc_utils
open Zinc_types

(* Use `dune build -w @zinctest --no-buffer` to run just the zinc tests! *)
let test_interpreter_context =
  Zinc_types.{ get_contract_opt = (fun address -> Some (address, None)) }

(* Helpers *)

(* Compiling *)
let init_env = Environment.default Environment.Protocols.current

let to_zinc ~raise ~add_warning path =
  let typed, _ =
    Ligo_compile.Utils.type_file ~raise ~add_warning
      ~options:Test_helpers.options path "auto" Env
  in
  let zinc = Ligo_compile.Zinc_of_typed.compile ~raise typed in
  zinc

let blank_raise_and_warn f =
  f ~raise:Trace.{ raise = (fun _ -> assert false) } ~add_warning:(fun _ -> ())

(* Alcotest setup *)

let expect_program =
  Alcotest.(
    check
      (Alcotest.testable
         (fun ppf program -> Fmt.pf ppf "%a" pp_program program)
         equal_program))

let expect_code =
  Alcotest.(
    check
      (Alcotest.testable
         (fun ppf zinc -> Fmt.pf ppf "%a" Zinc.pp zinc)
         Zinc.equal))

let expect_env =
  Alcotest.(
    check
      (Alcotest.testable
         (fun ppf env -> Fmt.pf ppf "%a" pp_env env)
         equal_env))

let expect_stack =
  Alcotest.(
    check
      (Alcotest.testable
         (fun ppf stack -> Fmt.pf ppf "%a" pp_stack stack)
         equal_stack))

type test =
  raise:Main_errors.all Trace.raise ->
  add_warning:(Main_warnings.all -> unit) ->
  unit ->
  unit

let expect_simple_compile_to ?(dialect = Self_ast_imperative.Syntax.PascaLIGO) ?(index = 0)
    ?(initial_stack = []) ?expect_failure ?expected_output_env ?expected_output
    contract_file (expected_zinc : Zinc_types.program) : test =
 fun ~raise ~add_warning () ->
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
  match
    ( expect_failure,
      List.nth_exn zinc index |> snd
      |> Zinc_interpreter.initial_state ~initial_stack
      |> Zinc_interpreter.interpret_zinc test_interpreter_context )
  with
  | None, Success (output_env, output_stack) ->
      let () =
        match expected_output_env with
        | Some expected_zinc ->
            expect_env
              (Printf.sprintf "evaluating env for %s" contract_file)
              expected_zinc output_env
        | None -> ()
      in
      let () =
        match expected_output with
        | Some expected_stack ->
            expect_stack
              (Printf.sprintf "evaluating stack for %s" contract_file)
              expected_stack output_stack
        | None -> ()
      in
      ()
  | Some s, Failure s' -> Alcotest.(check string) "hmm" s s'
  | Some _, Success _ ->
      failwith "expected failure, but execution was successful"
  | None, Failure _ ->
      failwith "was not expecting failure, but execution failed anyway"

(* ================ *)
(* Tests *)

let simple_1 =
  expect_simple_compile_to "simple1"
    [ ("i", [  Plain_old_data (Num (Z.of_int 42)); Core Return ]) ]
    ~expected_output:[ Stack_item.Z (Plain_old_data (Num (Z.of_int 42))) ]

let simple_2 =
  expect_simple_compile_to "simple2"
    [ ("i", [ Plain_old_data (Num (Z.of_int 42)); Core Return ]) ]
    ~expected_output:[ Stack_item.Z (Plain_old_data (Num (Z.of_int 42))) ]

let simple_3 =
  expect_simple_compile_to "simple3"
    [
      ("my_address", [ Plain_old_data (Address "tz1KqTpEZ7Yob7QbPE4Hy4Wo8fHG8LhKxZSx"); Core Return ]);
    ]
    ~expected_output:
      [ Stack_item.Z (Plain_old_data (Address "tz1KqTpEZ7Yob7QbPE4Hy4Wo8fHG8LhKxZSx")) ]

let id =
  expect_simple_compile_to "id_func"
    [ ("id", [ Core Grab; Core (Access 0); Core Return ]) ]
    ~initial_stack:[ Stack_item.Z (Plain_old_data (Num (Z.of_int 42))) ]
    ~expected_output:[ Stack_item.Z (Plain_old_data (Num (Z.of_int 42))) ]

let chain_id =
  expect_simple_compile_to "chain_id"
    [ ("chain_id", [ Domain_specific_operation ChainID; Core Return ]) ]
    ~expected_output:[ Stack_item.Z (Plain_old_data (Hash "not sure yet")) ]

let chain_id_func =
  expect_simple_compile_to "chain_id_func"
    [ ("chain_id", [ Core Grab; Domain_specific_operation ChainID; Core Return ]) ]
    ~initial_stack:[ Zinc_types.Utils.unit_record_stack ]

let tuple_creation =
  expect_simple_compile_to "tuple_creation"
    [
      ( "dup",
        [
          Core Grab; Core (Access 0); Core (Access 0); Adt (MakeRecord 2); Core Return;
        ] );
    ]
    ~initial_stack:[ Stack_item.Z (Plain_old_data (Num Z.one)) ]
    ~expected_output:
      [
        Stack_item.Record
          Zinc_utils.LMap.(
            let one = Stack_item.Z (Plain_old_data (Num Z.one)) in
            empty |> add (0) one |> add (1) one);
      ]

let check_record_destructure =
  expect_simple_compile_to "check_record_destructure"
    [
      ( "check_record_destructure",
        [
          Core Grab;
          Core (Access 0);
          Core Grab;
          Core (Access 0);
          Core Grab;
          Core (Access 0);
          Adt (RecordAccess (1));
          Core Grab;
          Core (Access 1);
          Adt (RecordAccess (0));
          Core Grab;
          Core (Access 1);
          Core (Access 0);
          Operation Add;
          Core EndLet;
          Core EndLet;
          Core EndLet;
          Core Return;
        ] );
    ]
    ~initial_stack:
      [
        Stack_item.Record
          Zinc_utils.LMap.(
            let one = Stack_item.Z (Plain_old_data (Num Z.one)) in
            empty |> add (0) one |> add (1) one);
      ]

let check_hash_key =
  let open Zinc_utils in
  expect_simple_compile_to "key_hash"
    [
      ( "check_hash_key",
        [
          Core Grab;
          Core (Access 0);
          Core Grab;
          Core (Access 0);
          Core Grab;
          Core (Access 0);
          Adt (RecordAccess (1));
          Core Grab;
          Core (Access 1);
          Adt (RecordAccess (0));
          Core Grab;
          Core (Access 1);
          Operation HashKey;
          Core Grab;
          Core (Access 0);
          Core (Access 0);
          Core (Access 1);
          Operation Eq;
          Adt (MakeRecord 2);
          Core EndLet;
          Core EndLet;
          Core EndLet;
          Core EndLet;
          Core Return;
        ] );
    ]
    ~initial_stack:
      [
        Stack_item.Record
          (LMap.empty
          |> LMap.add (0)
               (Stack_item.Z (Plain_old_data (Hash "not sure yet")))
          |> LMap.add (1) (Stack_item.Z (Plain_old_data (Key "Hashy hash!"))));
      ]

let basic_function_application =
  expect_simple_compile_to ~dialect:ReasonLIGO "basic_function_application"
    [ ("a", [ Plain_old_data (Num (Z.of_int 3)); Core Grab; Core (Access 0); Core Return ]) ]
    ~expected_output:[ Stack_item.Z (Plain_old_data (Num (Z.of_int 3))) ]

let basic_link =
  expect_simple_compile_to ~dialect:ReasonLIGO "basic_link"
    [
      ("a", [ Plain_old_data (Num (Z.of_int 1)); Core Return ]);
      ("b", [ Plain_old_data (Num (Z.of_int 1)); Core Grab; Core (Access 0); Core Return ]);
    ]
    ~index:1
    ~expected_output:[ Stack_item.Z (Plain_old_data (Num (Z.of_int 1))) ]

let failwith_simple =
  expect_simple_compile_to ~dialect:ReasonLIGO "failwith_simple"
    [ ("a", [ Plain_old_data (String "Not a contract"); Control_flow Failwith; Core Return ]) ]
    ~expect_failure:"Not a contract"

let get_contract_opt =
  expect_simple_compile_to ~dialect:ReasonLIGO "get_contract_opt"
    [ ("a", [ Plain_old_data (Address "whatever"); Domain_specific_operation Contract_opt; Core Return ]) ]
    ~expected_output:
      [
        Stack_item.Variant
          ("Some", Stack_item.NonliteralValue (Contract ("whatever", None)));
      ]

let match_on_sum =
  expect_simple_compile_to ~dialect:ReasonLIGO "match_on_sum"
    [
      ( "a",
        [
          Plain_old_data (Address "tz1TGu6TN5GSez2ndXXeDX6LgUDvLzPLqgYV");
          Core Grab;
          Core (Access 0);
          Domain_specific_operation Contract_opt;
          Core Grab;
          Core (Access 0);
          Adt (MatchVariant
            [
              ( "Some", [ Core Grab; Core (Access 0) ]);
              ("None", [ Core Grab; Plain_old_data (String "Not a contract"); Control_flow Failwith ]);
            ]);
          Core Return;
        ] );
    ]
    ~expected_output:
      [
        Stack_item.NonliteralValue
          (Contract ("tz1TGu6TN5GSez2ndXXeDX6LgUDvLzPLqgYV", None));
      ]

(* below this line are tests that fail because I haven't yet implemented the necessary primatives *)

let mutez_construction =
  expect_simple_compile_to ~dialect:ReasonLIGO "mutez_construction"
    [ ("a", [ Plain_old_data (Mutez (Z.of_int 1)); Core Return ]) ]

let create_transaction =
  expect_simple_compile_to ~dialect:ReasonLIGO "create_transaction"
    [
      ( "a",
        [
          Plain_old_data (Address "tz1TGu6TN5GSez2ndXXeDX6LgUDvLzPLqgYV");
          Core Grab;
          Core (Access 0);
          Domain_specific_operation Contract_opt;
          Core Grab;
          Core (Access 0);
          Adt (MatchVariant
            [
              ("Some", [ Core Grab; Core (Access 0) ]);
              ("None", [ Core Grab; Plain_old_data (String "Not a contract"); Control_flow Failwith ]);
            ]);
          Core EndLet;
          Core Grab;
          Core (Access 0);
          Plain_old_data (Mutez (Z.of_int 10));
          Adt (MakeRecord 0);
          Domain_specific_operation MakeTransaction;
          Core Return;
        ] );
    ]
    ~expected_output:
      [
        Stack_item.NonliteralValue
          (Chain_operation
             (Transaction
                (Z.of_int 10, ("tz1TGu6TN5GSez2ndXXeDX6LgUDvLzPLqgYV", None))));
      ]

let create_transaction_in_tuple =
  let open Zinc_utils in
  expect_simple_compile_to ~dialect:ReasonLIGO "create_transaction_in_tuple"
    [
      ( "a",
        [
          Plain_old_data (Address "tz1TGu6TN5GSez2ndXXeDX6LgUDvLzPLqgYV");
          Core Grab;
          Core (Access 0);
          Domain_specific_operation Contract_opt;
          Core Grab;
          Core (Access 0);
          Adt (MatchVariant
            [
              ("Some", [ Core Grab; Core (Access 0) ]);
              ("None", [ Core Grab; Plain_old_data (String "Not a contract"); Control_flow Failwith ]);
            ]);
          Core EndLet;
          Core Grab;
          Plain_old_data (String "my string");
          Plain_old_data (Key "edpkuBknW28nW72KG6RoHtYW7p12T6GKc7nAbwYX5m8Wd9sDVC9yav");
          Core (Access 0);
          Plain_old_data (Mutez (Z.of_int 10));
          Adt (MakeRecord 0);
          Domain_specific_operation MakeTransaction;
          Adt (MakeRecord 3);
          Core Return;
        ] );
    ]
    ~expected_output:
      [
        Stack_item.Record
          LMap.(
            empty
            |> add (0)
                 (Stack_item.NonliteralValue (Chain_operation
                          (Transaction
                             ( Z.of_int 10,
                               ("tz1TGu6TN5GSez2ndXXeDX6LgUDvLzPLqgYV", None) ))))
            |> add (1)
                 (Stack_item.Z
                    (Plain_old_data (Key
                       "edpkuBknW28nW72KG6RoHtYW7p12T6GKc7nAbwYX5m8Wd9sDVC9yav")))
            |> add (2) (Stack_item.Z (Plain_old_data (String "my string"))));
      ]

let list_construction =
  expect_simple_compile_to ~dialect:ReasonLIGO "list_construction" 
  [("a", [Plain_old_data Nil; Plain_old_data (Mutez (Z.of_int 2)); Operation Cons; Plain_old_data (Mutez (Z.of_int 1)); Operation Cons; Core Return])]
  ~expected_output:[
    (Zinc_types.Stack_item.List
      [ 
        (Zinc_types.Stack_item.Z (Plain_old_data (Mutez (Z.of_int 1))));
        (Zinc_types.Stack_item.Z (Plain_old_data (Mutez (Z.of_int 2))))
      ])
  ]


let bools_religo =
  expect_simple_compile_to ~dialect:ReasonLIGO "bools"
    [("a", [(Plain_old_data (Bool true)); (Core Return)]);
                ("b",
                 [(Plain_old_data (Bool true)); (Core Grab);
                   (Plain_old_data (Bool false)); (Core Return)]);
                ("lf",
                 [(Plain_old_data (Bool false)); (Core Grab);
                   (Plain_old_data (Bool true)); (Core Grab);
                   (Plain_old_data (Bool false)); (Core Grab);
                   (Core (Access 0)); (Core Grab); (Core (Access 0));
                   (Adt
                      (MatchVariant
                         [("False", [(Core Grab); (Core (Access 3))]);
                           ("True", [(Core Grab); (Core (Access 3))])]));
                   (Core Return)])
                ]
    ~expected_output:[(Zinc_types.Stack_item.Z (Plain_old_data (Bool true)))]

let bools_ligo =
  expect_simple_compile_to ~dialect:PascaLIGO "bools"
    [("a", [(Plain_old_data (Bool true)); (Core Return)]);
                ("b",
                 [(Plain_old_data (Bool true)); (Core Grab);
                   (Plain_old_data (Bool false)); (Core Return)]);
                ("lf",
                 [(Plain_old_data (Bool false)); (Core Grab);
                   (Plain_old_data (Bool true)); (Core Grab); (Core Grab);
                   (Core (Access 0)); (Core Grab); (Core (Access 0));
                   (Adt
                      (MatchVariant
                         [("False", [(Core Grab); (Core (Access 3))]);
                           ("True", [(Core Grab); (Core (Access 3))])]));
                   (Core Return)])
                ]
    ~expected_output:[(Zinc_types.Stack_item.Z (Plain_old_data (Bool true)))]

let bool_ops =
  expect_simple_compile_to ~dialect:PascaLIGO "boolean_operators"
    [("or_true",
                [(Core Grab); (Plain_old_data (Bool true));
                  (Core (Access 0)); (Operation Or); (Core Return)]);
                ("or_false",
                 [(Core
                     (Closure
                        [(Core Grab); (Plain_old_data (Bool true));
                          (Core (Access 0)); (Operation Or); (Core Return)]));
                   (Core Grab); (Core Grab); (Plain_old_data (Bool false));
                   (Core (Access 0)); (Operation Or); (Core Return)]);
                ("and_true",
                 [(Core
                     (Closure
                        [(Core Grab); (Plain_old_data (Bool false));
                          (Core (Access 0)); (Operation Or); (Core Return)]));
                   (Core Grab);
                   (Core
                      (Closure
                         [(Core Grab); (Plain_old_data (Bool true));
                           (Core (Access 0)); (Operation Or); (Core Return)]));
                   (Core Grab); (Core Grab); (Plain_old_data (Bool true));
                   (Core (Access 0)); (Operation And); (Core Return)]);
                ("and_false",
                 [(Core
                     (Closure
                        [(Core Grab); (Plain_old_data (Bool true));
                          (Core (Access 0)); (Operation And); (Core Return)]));
                   (Core Grab);
                   (Core
                      (Closure
                         [(Core Grab); (Plain_old_data (Bool false));
                           (Core (Access 0)); (Operation Or); (Core Return)]));
                   (Core Grab);
                   (Core
                      (Closure
                         [(Core Grab); (Plain_old_data (Bool true));
                           (Core (Access 0)); (Operation Or); (Core Return)]));
                   (Core Grab); (Core Grab); (Plain_old_data (Bool false));
                   (Core (Access 0)); (Operation And); (Core Return)]);
                ("not_bool",
                 [(Core
                     (Closure
                        [(Core Grab); (Plain_old_data (Bool false));
                          (Core (Access 0)); (Operation And); (Core Return)]));
                   (Core Grab);
                   (Core
                      (Closure
                         [(Core Grab); (Plain_old_data (Bool true));
                           (Core (Access 0)); (Operation And); (Core Return)]));
                   (Core Grab);
                   (Core
                      (Closure
                         [(Core Grab); (Plain_old_data (Bool false));
                           (Core (Access 0)); (Operation Or); (Core Return)]));
                   (Core Grab);
                   (Core
                      (Closure
                         [(Core Grab); (Plain_old_data (Bool true));
                           (Core (Access 0)); (Operation Or); (Core Return)]));
                   (Core Grab); (Core Grab); (Core (Access 0));
                   (Operation Not); (Core Return)])
                ]
    ~initial_stack:[ Stack_item.Z (Plain_old_data (Bool false)) ]
    ~expected_output:[ Stack_item.Z (Plain_old_data  (Bool true)) ]

let if_then_else_op =
  let open Z in 
  expect_simple_compile_to ~dialect:PascaLIGO "if_then_else_op"
    [("a", [(Plain_old_data (Num ~$2)); (Core Return)]);
                ("b",
                 [(Plain_old_data (Num ~$2)); (Core Grab);
                   (Plain_old_data (Num ~$3)); (Core Return)]);
                ("lf",
                 [(Plain_old_data (Num ~$3)); (Core Grab);
                   (Plain_old_data (Num ~$2)); (Core Grab); (Core Grab);
                   (Core (Access 0)); (Core Grab); (Core (Access 0));
                   (Core Grab); (Core (Access 0)); (Adt (RecordAccess 1));
                   (Core Grab); (Core (Access 1)); (Adt (RecordAccess 0));
                   (Core Grab); (Core (Access 1)); (Core (Access 0));
                   (Operation Or); (Core Grab); (Core (Access 0));
                   (Adt
                      (MatchVariant
                         [("False", [(Core Grab); (Core (Access 7))]);
                           ("True", [(Core Grab); (Core (Access 7))])]));
                   (Core EndLet); (Core EndLet); (Core EndLet);
                   (Core EndLet); (Core Return)])
                ]
    ~initial_stack:[ Stack_item.Z (Plain_old_data (Bool true)); Stack_item.Z (Plain_old_data (Bool true)) ]
    ~expected_output:
      [(Zinc_types.Stack_item.Z (Plain_old_data (Num ~$2)));
                (Zinc_types.Stack_item.Z (Plain_old_data (Bool true)));
                (Zinc_types.Stack_item.Z (Plain_old_data (Bool true)))]
let make_an_option =
  expect_simple_compile_to ~dialect:ReasonLIGO "make_an_option"
    [
      ("a", [ Adt (MakeRecord 0); Adt (MakeVariant "None"); Core Return ]);
      ( "b",
        [
          Adt (MakeRecord 0);
          (* constructing None, from the definition of a *)
          Adt (MakeVariant "None");
          Core Grab;
          Adt (MakeRecord 0);
          (* constructing Some *)
          Adt (MakeVariant "Some");
          Core Return;
        ] );
    ]

let make_a_custom_option =
  expect_simple_compile_to ~dialect:ReasonLIGO "make_a_custom_option"
    [
      ("a", [ Adt (MakeRecord 0); Adt (MakeVariant "My_none"); Core Return ]);
      ( "b",
        [
          Adt (MakeRecord 0);
          (* constructing My_none, from the definition of a *)
          Adt (MakeVariant ("My_none"));
          Core Grab;
          Adt (MakeRecord 0);
          (* constructing Some *)
          Adt (MakeVariant "My_some");
          Core Return;
        ] );
    ]

let main =
  let open Test_helpers in
  test_suite "Zinc tests"
    [
      test_w "simple1" simple_1;
      test_w "simple2" simple_2;
      test_w "simple3" simple_3;
      test_w "bools_religo" bools_religo;
      test_w "bools_ligo" bools_ligo;
      test_w "bool_ops" bool_ops;
      test_w "if with ops" if_then_else_op;
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
      
    ]
