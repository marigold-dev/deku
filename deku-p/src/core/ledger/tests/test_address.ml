open Deku_ledger

let addr = Alcotest.testable Address.pp Address.equal

let test_address_from_string () =
  let address = "tz1fpf9DffkGAnzT6UKMDoS4hZjNmoEKhGsK" in
  let address = Address.of_b58 address in
  let is_some = Option.is_some address in
  Alcotest.(check bool) "Address has been parsed" true is_some

let test_wrong_address_from_string () =
  let address = "ksGHeh4SfkGAnzT6UKMDoS4hZjNmoEKhGsK" in
  let address = Address.of_b58 address in
  let is_none = Option.is_none address in
  Alcotest.(check bool) "Address cannot be parsed" true is_none

let test_b58 () =
  let address_string = "tz1fpf9DffkGAnzT6UKMDoS4hZjNmoEKhGsK" in
  let address_b58 =
    address_string |> Address.of_b58 |> Option.get |> Address.to_b58
  in
  Alcotest.(check string)
    "of_b58 |> to_b58 should returns the input" address_string address_b58

let test_key_hash () =
  let open Deku_crypto in
  let raw_key_hash =
    Key_hash.of_b58 "tz1fpf9DffkGAnzT6UKMDoS4hZjNmoEKhGsK" |> Option.get
  in
  let key_hash =
    raw_key_hash |> Address.of_key_hash |> Address.to_key_hash |> Option.get
  in
  Alcotest.(check bool)
    "of_key_hash |> to_key_hash shouyld returns the input" true
    (Key_hash.equal raw_key_hash key_hash)

let test_yojson () =
  let raw_address =
    Address.of_b58 "tz1fpf9DffkGAnzT6UKMDoS4hZjNmoEKhGsK" |> Option.get
  in
  let address_yojson =
    raw_address |> Address.yojson_of_t |> Address.t_of_yojson
  in
  Alcotest.(check addr)
    "yojson_of |> of_yojson should returns the input" raw_address address_yojson

let test_address_equal () =
  let address =
    Address.of_b58 "tz1fpf9DffkGAnzT6UKMDoS4hZjNmoEKhGsK" |> Option.get
  in
  Alcotest.(check bool)
    "two same addresses must be equal" true
    (Address.equal address address)

let test_address_not_equal () =
  let address1 =
    Address.of_b58 "tz1fpf9DffkGAnzT6UKMDoS4hZjNmoEKhGsK" |> Option.get
  in
  let address2 =
    Address.of_b58 "tz1PYdVbnLwiqKo3fLFXTKxw6K7BhpddQPh8" |> Option.get
  in
  Alcotest.(check bool)
    "two different addresses must not be equal" false
    (Address.equal address1 address2)

let run () =
  let open Alcotest in
  run "Address" ~and_exit:false
    [
      ( "Repr",
        [
          test_case "parse a correct string address " `Quick
            test_address_from_string;
          test_case "parsed an incorrect string address" `Quick
            test_wrong_address_from_string;
          test_case "of_b58 is inverse of from_b58" `Quick test_b58;
          test_case "of_key_hash is the inverse of to_key_hash" `Quick
            test_key_hash;
          test_case "yojson_of is the inverse of of_yojson" `Quick test_yojson;
          test_case "two same addresses are equal" `Quick test_address_equal;
          test_case "two different addresses are not equal" `Quick
            test_address_not_equal;
        ] );
    ]
