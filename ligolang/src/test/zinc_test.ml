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
         (fun ppf program -> Fmt.pf ppf "%a" Zinc_types.pp_program program)
         Zinc_types.equal_program))

let expect_code =
  Alcotest.(
    check
      (Alcotest.testable
         (fun ppf zinc -> Fmt.pf ppf "%a" Zinc_types.pp_zinc_code zinc)
         Zinc_types.equal_zinc_code))

let expect_env =
  Alcotest.(
    check
      (Alcotest.testable
         (fun ppf env -> Fmt.pf ppf "%a" Zinc_types.pp_env env)
         Zinc_types.equal_env))

let expect_stack =
  Alcotest.(
    check
      (Alcotest.testable
         (fun ppf stack -> Fmt.pf ppf "%a" Zinc_types.pp_stack stack)
         Zinc_types.equal_stack))

type test =
  raise:Main_errors.all Trace.raise ->
  add_warning:(Main_warnings.all -> unit) ->
  unit ->
  unit

let expect_simple_compile_to ?reason:(enabled = false) ?(index = 0)
    ?(initial_stack = []) ?expect_failure ?expected_output_env ?expected_output
    contract_file (expected_zinc : Zinc_types.program) : test =
 fun ~raise ~add_warning () ->
  let to_zinc = to_zinc ~raise ~add_warning in
  let contract =
    Printf.sprintf "./contracts/%s.%s" contract_file
      (if enabled then "religo" else "ligo")
  in
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
    [ ("i", [ Num (Z.of_int 42); Return ]) ]
    ~expected_output:[ Stack_item.Z (Num (Z.of_int 42)) ]

let simple_2 =
  expect_simple_compile_to "simple2"
    [ ("i", [ Num (Z.of_int 42); Return ]) ]
    ~expected_output:[ Stack_item.Z (Num (Z.of_int 42)) ]

let simple_3 =
  expect_simple_compile_to "simple3"
    [
      ("my_address", [ Address "tz1KqTpEZ7Yob7QbPE4Hy4Wo8fHG8LhKxZSx"; Return ]);
    ]
    ~expected_output:[ Stack_item.Z (Address "tz1KqTpEZ7Yob7QbPE4Hy4Wo8fHG8LhKxZSx") ]

let id =
  expect_simple_compile_to "id_func"
    [ ("id", [ Grab; Access 0; Return ]) ]
    ~initial_stack:[ Stack_item.Z (Num (Z.of_int 42)) ]
    ~expected_output:[ Stack_item.Z (Num (Z.of_int 42)) ]

let chain_id =
  expect_simple_compile_to "chain_id"
    [ ("chain_id", [ ChainID; Return ]) ]
    ~expected_output:[ Stack_item.Z (Zinc_types.Hash "not sure yet") ]

let chain_id_func =
  expect_simple_compile_to "chain_id_func"
    [ ("chain_id", [ Grab; ChainID; Return ]) ]
    ~initial_stack:[ Zinc_types.Utils.unit_record_stack ]

let tuple_creation =
  expect_simple_compile_to "tuple_creation"
    [
      ( "dup",
        [
          Grab; Access 0; Access 0; MakeRecord [ Label "0"; Label "1" ]; Return;
        ] );
    ]
    ~initial_stack:[ Stack_item.Z (Num Z.one) ]
    ~expected_output:
      [
        Stack_item.Record
          Zinc_utils.LMap.(
            let one = Stack_item.Z (Num Z.one) in
            empty |> add (Label "0") one |> add (Label "1") one);
      ]

let check_record_destructure =
  expect_simple_compile_to "check_record_destructure"
    [
      ( "check_record_destructure",
        [
          Grab;
          Access 0;
          Grab;
          Access 0;
          Grab;
          Access 0;
          RecordAccess (Label "1");
          Grab;
          Access 1;
          RecordAccess (Label "0");
          Grab;
          Access 1;
          Access 0;
          Add;
          EndLet;
          EndLet;
          EndLet;
          Return;
        ] );
    ]
    ~initial_stack:
      [
        Stack_item.Record
          Zinc_utils.LMap.(
            let one = Stack_item.Z (Num Z.one) in
            empty |> add (Label "0") one |> add (Label "1") one);
      ]

let check_hash_key =
  let open Zinc_utils in
  expect_simple_compile_to "key_hash"
    [
      ( "check_hash_key",
        [
          Grab;
          Access 0;
          Grab;
          Access 0;
          Grab;
          Access 0;
          RecordAccess (Label "1");
          Grab;
          Access 1;
          RecordAccess (Label "0");
          Grab;
          Access 1;
          HashKey;
          Grab;
          Access 0;
          Access 0;
          Access 1;
          Eq;
          MakeRecord [ Label "0"; Label "1" ];
          EndLet;
          EndLet;
          EndLet;
          EndLet;
          Return;
        ] );
    ]
    ~initial_stack:
      [
        Stack_item.Record
          (LMap.empty
          |> LMap.add (Label "0") (Stack_item.Z (Zinc_types.Hash "not sure yet"))
          |> LMap.add (Label "1") (Stack_item.Z (Key "Hashy hash!")));
      ]

let basic_function_application =
  expect_simple_compile_to ~reason:true "basic_function_application"
    [ ("a", [ Num (Z.of_int 3); Grab; Access 0; Return ]) ]
    ~expected_output:[ Stack_item.Z (Num (Z.of_int 3)) ]

let basic_link =
  expect_simple_compile_to ~reason:true "basic_link"
    [
      ("a", [ Num (Z.of_int 1); Return ]);
      ("b", [ Num (Z.of_int 1); Grab; Access 0; Return ]);
    ]
    ~index:1
    ~expected_output:[ Stack_item.Z (Num (Z.of_int 1)) ]

let failwith_simple =
  expect_simple_compile_to ~reason:true "failwith_simple"
    [ ("a", [ String "Not a contract"; Failwith; Return ]) ]
    ~expect_failure:"Not a contract"

let get_contract_opt =
  expect_simple_compile_to ~reason:true "get_contract_opt"
    [ ("a", [ Address "whatever"; Contract_opt; Return ]) ]
    ~expected_output:
      [ Stack_item.Variant (Label "Some", Stack_item.Z (Extensions (Contract ("whatever", None)))) ]

let match_on_sum =
  expect_simple_compile_to ~reason:true "match_on_sum"
    [
      ( "a",
        [
          Address "tz1TGu6TN5GSez2ndXXeDX6LgUDvLzPLqgYV";
          Grab;
          Access 0;
          Contract_opt;
          Grab;
          Access 0;
          MatchVariant
            [
              (Label "Some", [ Grab; Access 0 ]);
              (Label "None", [ Grab; String "Not a contract"; Failwith ]);
            ];
          Return;
        ] );
    ]
    ~expected_output:
      [
        Stack_item.Z
          (Extensions (Contract ("tz1TGu6TN5GSez2ndXXeDX6LgUDvLzPLqgYV", None)));
      ]

(* below this line are tests that fail because I haven't yet implemented the necessary primatives *)

let mutez_construction =
  expect_simple_compile_to ~reason:true "mutez_construction"
    [ ("a", [ Mutez (Z.of_int 1); Return ]) ]

let create_transaction =
  expect_simple_compile_to ~reason:true "create_transaction"
    [
      ( "a",
        [
          Address "tz1TGu6TN5GSez2ndXXeDX6LgUDvLzPLqgYV";
          Grab;
          Access 0;
          Contract_opt;
          Grab;
          Access 0;
          MatchVariant
            [
              (Label "Some", [ Grab; Access 0 ]);
              (Label "None", [ Grab; String "Not a contract"; Failwith ]);
            ];
          EndLet;
          Grab;
          Access 0;
          Mutez (Z.of_int 10);
          MakeRecord [];
          MakeTransaction;
          Return;
        ] );
    ]
    ~expected_output:
      [
        Stack_item.Z
          (Extensions
             (Operation
                (Transaction
                   (Z.of_int 10, ("tz1TGu6TN5GSez2ndXXeDX6LgUDvLzPLqgYV", None)))));
      ]

let create_transaction_in_tuple =
  let open Zinc_utils in
  expect_simple_compile_to ~reason:true "create_transaction_in_tuple"
    [
      ( "a",
        [
          Address "tz1TGu6TN5GSez2ndXXeDX6LgUDvLzPLqgYV";
          Grab;
          Access 0;
          Contract_opt;
          Grab;
          Access 0;
          MatchVariant
            [
              (Label "Some", [ Grab; Access 0 ]);
              (Label "None", [ Grab; String "Not a contract"; Failwith ]);
            ];
          EndLet;
          Grab;
          String "my string";
          Key "edpkuBknW28nW72KG6RoHtYW7p12T6GKc7nAbwYX5m8Wd9sDVC9yav";
          Access 0;
          Mutez (Z.of_int 10);
          MakeRecord [];
          MakeTransaction;
          MakeRecord [ Label "0"; Label "1"; Label "2" ];
          Return;
        ] );
    ]
    ~expected_output:
      [
        Stack_item.Record
          LMap.(
            empty
            |> add (Label "0")
                 (Stack_item.Z
                   (Extensions
                      (Operation
                         (Transaction
                            ( Z.of_int 10,
                              ("tz1TGu6TN5GSez2ndXXeDX6LgUDvLzPLqgYV", None) )))))
            |> add (Label "1")
                 (Stack_item.Z
                   (Key "edpkuBknW28nW72KG6RoHtYW7p12T6GKc7nAbwYX5m8Wd9sDVC9yav"))
            |> add (Label "2") (Stack_item.Z (String "my string")));
      ]

let list_construction =
  expect_simple_compile_to ~reason:true "list_construction" []
(* necessary zinc_types primitives not yet implemented *)

let main =
  let open Test_helpers in
  test_suite "Zinc tests"
    [
      test_w "simple1" simple_1;
      test_w "simple2" simple_2;
      test_w "simple3" simple_3;
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
    ]
