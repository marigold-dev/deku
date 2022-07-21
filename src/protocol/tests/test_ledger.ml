open Deku_stdlib
open Deku_crypto
open Deku_concepts
open Deku_protocol
open Ledger

let total_balance (ledger : ledger) =
  Address.Map.fold
    (fun _address amount total_amount -> Amount.(amount + total_amount))
    (ledger :> Amount.t Address.Map.t)
    Amount.zero

let new_address () =
  let secret = Ed25519.Secret.generate () in
  let secret = Secret.Ed25519 secret in
  let key = Key.of_secret secret in
  let key_hash = Key_hash.of_key key in
  Address.of_key_hash key_hash

let amount_of_int n =
  let n = Option.get (N.of_z (Z.of_int n)) in
  Amount.of_n n

let random_amount () =
  let amount = Int32.to_int (Random.int32 16l) in
  amount_of_int amount

let amount = Alcotest.testable Amount.pp Amount.equal

let test_total_balance ~expected ~ledger_name ~ledger () =
  let msg =
    Format.asprintf "total_balance %s = %a" ledger_name Amount.pp expected
  in
  Alcotest.(check' amount) ~msg ~expected ~actual:(total_balance ledger)

let test_balance ~address_name ~address ~expected ~ledger_name ~ledger () =
  let msg =
    Format.asprintf "balance %s %s = %a" address_name ledger_name Amount.pp
      expected
  in
  Alcotest.(check' amount) ~msg ~expected ~actual:(balance address ledger)

let test_unknown_balance ~ledger_name ~ledger () =
  let unknown = new_address () in
  test_balance ~address_name:"unknown" ~address:unknown ~expected:Amount.zero
    ~ledger_name ~ledger ()

let test_balance ~address_name ~address ~expected ~ledger_name ~ledger () =
  let () =
    test_balance ~address_name ~address ~expected ~ledger_name ~ledger ()
  in
  let () = test_unknown_balance ~ledger_name ~ledger () in
  ()

let test_base_deposit ~address_name ~address ~amount ~ledger_name ~ledger () =
  let ledger_balance = total_balance ledger in
  let address_balance = balance address ledger in
  let ledger = deposit address amount ledger in

  let ledger_name =
    Format.asprintf "%s[%s += %a]" ledger_name address_name Amount.pp amount
  in

  let () =
    test_balance ~address_name ~address
      ~expected:Amount.(address_balance + amount)
      ~ledger_name ~ledger ()
  in
  let () =
    test_total_balance
      ~expected:Amount.(ledger_balance + amount)
      ~ledger_name ~ledger ()
  in
  let () = test_unknown_balance ~ledger_name ~ledger () in
  ()

let test_unknown_deposit ~ledger_name ~ledger () =
  let unknown = new_address () in
  let amount = random_amount () in
  test_base_deposit ~address_name:"unknown" ~address:unknown ~amount
    ~ledger_name ~ledger ()

let test_deposit ~address_name ~address ~amount ~ledger_name ~ledger () =
  let () =
    test_base_deposit ~address_name ~address ~amount ~ledger_name ~ledger ()
  in
  let () = test_unknown_deposit ~ledger_name ~ledger () in
  ()

let test_success_transfer ~sender_name ~sender ~expected_sender_balance
    ~receiver_name ~receiver ~amount ~ledger_name ~ledger () =
  let ledger_balance = total_balance ledger in
  let receiver_balance = balance receiver ledger in
  let msg =
    Format.asprintf
      "Option.is_some (transfer ~sender:%s ~receiver:%s %a %s) = true"
      sender_name receiver_name Amount.pp amount ledger_name
  in
  let () =
    Alcotest.(check' bool)
      ~msg ~expected:true
      ~actual:(Option.is_some (transfer ~sender ~receiver amount ledger))
  in
  let ledger = transfer ~sender ~receiver amount ledger in
  let ledger = Option.get ledger in

  let ledger_name =
    Format.asprintf "%s[%s -[%a]> %s]" ledger_name sender_name Amount.pp amount
      receiver_name
  in

  (* TODO: ensure that no other balance changed *)
  let () =
    test_balance ~address_name:sender_name ~address:sender
      ~expected:expected_sender_balance ~ledger_name ~ledger ()
  in
  let () =
    test_balance ~address_name:receiver_name ~address:receiver
      ~expected:Amount.(receiver_balance + amount)
      ~ledger_name ~ledger ()
  in
  let () =
    test_total_balance ~expected:ledger_balance ~ledger_name ~ledger ()
  in
  ()

let test_base_transfer ~sender_name ~sender ~receiver_name ~receiver ~amount
    ~ledger_name ~ledger () =
  let sender_balance = balance sender ledger in
  match Amount.(sender_balance - amount) with
  | Some expected_sender_balance ->
      test_success_transfer ~sender_name ~sender ~expected_sender_balance
        ~receiver_name ~receiver ~amount ~ledger_name ~ledger ()
  | None ->
      let msg =
        Format.asprintf
          "Option.is_none (transfer ~sender:%s ~receiver:%s %a %s) = true"
          sender_name receiver_name Amount.pp amount ledger_name
      in
      Alcotest.(check' bool)
        ~msg ~expected:true
        ~actual:(Option.is_none (transfer ~sender ~receiver amount ledger))

let test_unknown_transfer ~ledger_name ~ledger () =
  let unknown_a = new_address () in
  let unknown_b = new_address () in

  let amount = random_amount () in
  let ledger = deposit unknown_a amount ledger in
  let ledger_name =
    Format.asprintf "%s[unknown_a += %a]" ledger_name Amount.pp amount
  in
  test_success_transfer ~sender_name:"unknown_a" ~sender:unknown_a
    ~expected_sender_balance:Amount.zero ~receiver_name:"unknown_b"
    ~receiver:unknown_b ~amount ~ledger_name ~ledger ()

let test_transfer ~sender_name ~sender ~receiver_name ~receiver ~amount
    ~ledger_name ~ledger () =
  let () =
    test_base_transfer ~sender_name ~sender ~receiver_name ~receiver ~amount
      ~ledger_name ~ledger ()
  in
  let () = test_unknown_transfer ~ledger_name ~ledger () in
  ()

let test_deposit_and_transfer ~address_name ~address ~address_balance
    ~ledger_name ~ledger () =
  let third = new_address () in
  let () =
    test_deposit ~address_name:"third" ~address:third ~amount:(random_amount ())
      ~ledger_name ~ledger ()
  in
  let () =
    test_transfer ~sender_name:address_name ~sender:address
      ~receiver_name:"third" ~receiver:third ~amount:address_balance
      ~ledger_name ~ledger ()
  in
  ()

let test_initial_total_balance () =
  test_total_balance ~expected:Amount.zero ~ledger_name:"initial"
    ~ledger:initial ()

let test_unknown_balance_on_initial () =
  test_unknown_balance ~ledger_name:"initial" ~ledger:initial ()

let test_unknown_deposit_on_initial () =
  test_unknown_deposit ~ledger_name:"initial" ~ledger:initial ()

let test_unknown_transfer_on_initial () =
  test_unknown_transfer ~ledger_name:"initial" ~ledger:initial ()

let test_simple_deposit () =
  let a = new_address () in

  let amount = random_amount () in
  let ledger_with_a = deposit a amount initial in

  let () =
    test_total_balance ~expected:amount ~ledger_name:"ledger_with_a"
      ~ledger:ledger_with_a ()
  in
  let () =
    test_balance ~address_name:"a" ~address:a ~expected:amount
      ~ledger_name:"ledger_with_a" ~ledger:ledger_with_a ()
  in
  let () =
    test_deposit_and_transfer ~address_name:"a" ~address:a
      ~address_balance:amount ~ledger_name:"ledger_with_a" ~ledger:ledger_with_a
      ()
  in
  ()

let test_simple_transfer () =
  let a = new_address () in
  let b = new_address () in
  let total_amount = amount_of_int 6 in
  let ledger_with_a = deposit a total_amount initial in
  let ledger_with_a_and_b =
    transfer ~sender:a ~receiver:b (amount_of_int 4) ledger_with_a
  in
  let () =
    Alcotest.(check' bool)
      ~msg:
        "Option.is_some (transfer ~sender:a ~receiver:b 6 ledger_with_a) = true"
      ~expected:true
      ~actual:
        (Option.is_some
           (transfer ~sender:a ~receiver:b (amount_of_int 4) ledger_with_a))
  in
  let ledger_with_a_and_b = Option.get ledger_with_a_and_b in

  let () =
    test_total_balance ~expected:total_amount ~ledger_name:"ledger_with_a"
      ~ledger:ledger_with_a ()
  in
  let () =
    test_balance ~address_name:"a" ~address:a ~expected:(amount_of_int 2)
      ~ledger_name:"ledger_with_a_and_b" ~ledger:ledger_with_a_and_b ()
  in
  let () =
    test_balance ~address_name:"b" ~address:b ~expected:(amount_of_int 4)
      ~ledger_name:"ledger_with_a_and_b" ~ledger:ledger_with_a_and_b ()
  in
  let () =
    test_deposit_and_transfer ~address_name:"a" ~address:a
      ~address_balance:(amount_of_int 2) ~ledger_name:"ledger_with_a"
      ~ledger:ledger_with_a_and_b ()
  in
  let () =
    test_deposit_and_transfer ~address_name:"b" ~address:b
      ~address_balance:(amount_of_int 4) ~ledger_name:"ledger_with_a"
      ~ledger:ledger_with_a_and_b ()
  in
  ()

let run () =
  let open Alcotest in
  run "Ledger" ~and_exit:false
    [
      ( "general",
        [
          test_case "initial_total_balance" `Quick test_initial_total_balance;
          test_case "unknown_balance_on_initial" `Quick
            test_unknown_balance_on_initial;
          test_case "unknown_deposit_on_initial" `Quick
            test_unknown_deposit_on_initial;
          test_case "unknown_transfer_on_initial" `Quick
            test_unknown_transfer_on_initial;
          test_case "simple_deposit" `Quick test_simple_deposit;
          test_case "simple_transfer" `Quick test_simple_transfer;
        ] );
    ]
