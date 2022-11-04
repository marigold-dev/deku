open Deku_protocol

let correct_operation_hash =
  "Do2YxhRwd1bK5H9A5ULe6eS2bxsMsA8hgVbCxvbpSLQcyW9bkV5S"

let incorrect_operation_hash = "tz1fpf9DffkGAnzT6UKMDoS4hZjNmoEKhGsK"

let test_operation_hash_from_correct_string () =
  let operation_hash = Operation_hash.of_b58 correct_operation_hash in
  let is_some = Option.is_some operation_hash in
  Alcotest.(check bool) "Operation has been parsed" true is_some

let test_operation_hash_from_incorrect_string () =
  let operation_hash = Operation_hash.of_b58 incorrect_operation_hash in
  let is_none = Option.is_none operation_hash in
  Alcotest.(check bool) "Operation has not been parsed" true is_none

let test_b58 () =
  let result =
    correct_operation_hash |> Operation_hash.of_b58 |> Option.get
    |> Operation_hash.to_b58
  in
  Alcotest.(check string) "b58 should be the same" correct_operation_hash result

let test_prefix () =
  let operation_hash = Operation_hash.hash "hello" in
  let operation = Operation_hash.to_b58 operation_hash in
  Alcotest.(check bool)
    "b58 should start by Do" true
    (String.starts_with ~prefix:"Do" operation)

let test_equal () =
  let operation_1 = Operation_hash.hash "hello" in
  let operation_2 = Operation_hash.hash "hello" in
  Alcotest.(check bool)
    "should be equal" true
    (Operation_hash.equal operation_1 operation_2)

let test_not_equal () =
  let operation_1 = Operation_hash.hash "hello" in
  let operation_2 = Operation_hash.hash "world" in
  Alcotest.(check bool)
    "should be equal" false
    (Operation_hash.equal operation_1 operation_2)

let run () =
  let open Alcotest in
  run "Operation hash" ~and_exit:false
    [
      ( "Repr",
        [
          test_case "parse a correct operation address" `Quick
            test_operation_hash_from_correct_string;
          test_case "parse an incorrect operation address" `Quick
            test_operation_hash_from_incorrect_string;
          test_case "of_b58 is inverse of to_b58" `Quick test_b58;
          test_case "Do is the prefix of an operation hash" `Quick test_prefix;
          test_case "Two same operation hash should be equal" `Quick test_equal;
          test_case "Two different operation hash should not be equal" `Quick
            test_not_equal;
        ] );
    ]
