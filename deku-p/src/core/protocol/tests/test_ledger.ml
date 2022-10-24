open Deku_stdlib
open Deku_crypto
open Deku_concepts
open Deku_ledger
open Ledger

let total_balance ~ticket_id (Ledger { table; _ }) =
  let open Ticket_table in
  Address_map.fold
    (fun _sender ticket_map total_amount ->
      Ticket_map.find_opt ticket_id ticket_map
      |> Option.value ~default:Amount.zero
      |> Amount.( + ) total_amount)
    (table :> Amount.t Ticket_map.t Address_map.t)
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

let random_ticket_id () =
  let random_bytes length =
    Bytes.init length (fun _ -> Stdlib.Random.int 255 |> Char.chr)
  in
  (* TODO Should we expose Contract_hash.of_raw? *)
  let address : Deku_tezos.Contract_hash.t =
    random_bytes 20 |> String.of_bytes |> Deku_crypto.BLAKE2b.BLAKE2b_160.of_raw
    |> Option.get |> Obj.magic
  in
  let data = random_bytes 10 in
  Ticket_id.make (Tezos address) data

let test_total_balance ~expected ~ledger_name ~ticket_id ~ledger () =
  let msg =
    Format.asprintf "total_balance %s = %a" ledger_name Amount.pp expected
  in
  Alcotest.(check' amount)
    ~msg ~expected
    ~actual:(total_balance ~ticket_id ledger)

let test_balance ~address_name ~address ~expected ~ledger_name ~ticket_id
    ~ledger () =
  let msg =
    Format.asprintf "balance %s %s = %a" address_name ledger_name Amount.pp
      expected
  in
  Alcotest.(check' amount)
    ~msg ~expected
    ~actual:(balance address ticket_id ledger)

let test_unknown_balance ~ledger_name ~ledger () =
  let unknown = new_address () in
  test_balance ~address_name:"unknown" ~address:unknown ~expected:Amount.zero
    ~ledger_name ~ledger ()

let test_balance ~address_name ~address ~expected ~ledger_name ~ticket_id
    ~ledger () =
  let () =
    test_balance ~address_name ~address ~expected ~ledger_name ~ticket_id
      ~ledger ()
  in
  let () = test_unknown_balance ~ledger_name ~ticket_id ~ledger () in
  ()

let test_base_deposit ~address_name ~address ~amount ~ledger_name ~ticket_id
    ~ledger () =
  let ledger_balance = total_balance ~ticket_id ledger in
  let address_balance = balance address ticket_id ledger in
  let ledger = deposit address amount ticket_id ledger in

  let ledger_name =
    Format.asprintf "%s[%s += %a]" ledger_name address_name Amount.pp amount
  in

  let () =
    test_balance ~address_name ~address
      ~expected:Amount.(address_balance + amount)
      ~ledger_name ~ticket_id ~ledger ()
  in
  let () =
    test_total_balance
      ~expected:Amount.(ledger_balance + amount)
      ~ledger_name ~ticket_id ~ledger ()
  in
  let () = test_unknown_balance ~ledger_name ~ticket_id ~ledger () in
  ()

let test_unknown_deposit ~ledger_name ~ledger () =
  let unknown = new_address () in
  let amount = random_amount () in
  test_base_deposit ~address_name:"unknown" ~address:unknown ~amount
    ~ledger_name ~ledger ()

let test_deposit ~address_name ~address ~amount ~ledger_name ~ticket_id ~ledger
    () =
  let () =
    test_base_deposit ~address_name ~address ~amount ~ledger_name ~ticket_id
      ~ledger ()
  in
  let () = test_unknown_deposit ~ledger_name ~ticket_id ~ledger () in
  ()

let test_success_transfer ~sender_name ~sender ~expected_sender_balance
    ~receiver_name ~receiver ~amount ~ledger_name ~ticket_id ~ledger () =
  let ledger_balance = total_balance ~ticket_id ledger in
  let receiver_balance = balance receiver ticket_id ledger in
  let msg =
    Format.asprintf
      "Result.is_ok (transfer ~sender:%s ~receiver:%s %a %s) = true" sender_name
      receiver_name Amount.pp amount ledger_name
  in
  let () =
    Alcotest.(check' bool)
      ~msg ~expected:true
      ~actual:
        (Result.is_ok (transfer ~sender ~receiver ~amount ~ticket_id ledger))
  in
  let ledger = transfer ~sender ~receiver ~amount ~ticket_id ledger in
  let ledger = Result.get_ok ledger in

  let ledger_name =
    Format.asprintf "%s[%s -[%a]> %s]" ledger_name sender_name Amount.pp amount
      receiver_name
  in

  (* TODO: ensure that no other balance changed *)
  let () =
    test_balance ~address_name:sender_name ~address:sender
      ~expected:expected_sender_balance ~ledger_name ~ticket_id ~ledger ()
  in
  let () =
    test_balance ~address_name:receiver_name ~address:receiver
      ~expected:Amount.(receiver_balance + amount)
      ~ledger_name ~ticket_id ~ledger ()
  in
  let () =
    test_total_balance ~expected:ledger_balance ~ledger_name ~ticket_id ~ledger
      ()
  in
  ()

let test_base_transfer ~sender_name ~sender ~receiver_name ~receiver ~amount
    ~ledger_name ~ticket_id ~ledger () =
  let sender_balance = balance sender ticket_id ledger in
  match Amount.(sender_balance - amount) with
  | Some expected_sender_balance ->
      test_success_transfer ~sender_name ~sender ~expected_sender_balance
        ~receiver_name ~receiver ~amount ~ledger_name ~ticket_id ~ledger ()
  | None ->
      let msg =
        Format.asprintf
          "Option.is_none (transfer ~sender:%s ~receiver:%s %a %s) = true"
          sender_name receiver_name Amount.pp amount ledger_name
      in
      Alcotest.(check' bool)
        ~msg ~expected:true
        ~actual:
          (Result.is_ok (transfer ~sender ~receiver ~amount ~ticket_id ledger))

let test_unknown_transfer ~ledger_name ~ticket_id ~ledger () =
  let unknown_a = new_address () in
  let unknown_b = new_address () in

  let amount = random_amount () in
  let ledger = deposit unknown_a amount ticket_id ledger in
  let ledger_name =
    Format.asprintf "%s[unknown_a += %a]" ledger_name Amount.pp amount
  in
  test_success_transfer ~sender_name:"unknown_a" ~sender:unknown_a
    ~expected_sender_balance:Amount.zero ~receiver_name:"unknown_b"
    ~receiver:unknown_b ~amount ~ledger_name ~ticket_id ~ledger ()

let test_transfer ~sender_name ~sender ~receiver_name ~receiver ~amount
    ~ledger_name ~ticket_id ~ledger () =
  let () =
    test_base_transfer ~sender_name ~sender ~receiver_name ~receiver ~amount
      ~ledger_name ~ticket_id ~ledger ()
  in
  let () = test_unknown_transfer ~ledger_name ~ticket_id ~ledger () in
  ()

let test_deposit_and_transfer ~address_name ~address ~address_balance
    ~ledger_name ~ticket_id ~ledger () =
  let third = new_address () in
  let () =
    test_deposit ~address_name:"third" ~address:third ~amount:(random_amount ())
      ~ledger_name ~ticket_id ~ledger ()
  in
  let () =
    test_transfer ~sender_name:address_name ~sender:address
      ~receiver_name:"third" ~receiver:third ~amount:address_balance
      ~ledger_name ~ticket_id ~ledger ()
  in
  ()

let test_initial_total_balance () =
  test_total_balance ~expected:Amount.zero ~ledger_name:"initial"
    ~ticket_id:(random_ticket_id ()) ~ledger:initial ()

let test_unknown_balance_on_initial () =
  test_unknown_balance ~ledger_name:"initial" ~ticket_id:(random_ticket_id ())
    ~ledger:initial ()

let test_unknown_deposit_on_initial () =
  test_unknown_deposit ~ledger_name:"initial" ~ticket_id:(random_ticket_id ())
    ~ledger:initial ()

let test_unknown_transfer_on_initial () =
  test_unknown_transfer ~ledger_name:"initial" ~ticket_id:(random_ticket_id ())
    ~ledger:initial ()

let test_simple_deposit () =
  let a = new_address () in

  let ticket_id = random_ticket_id () in
  let amount = random_amount () in
  let ledger_with_a = deposit a amount ticket_id initial in

  let () =
    test_total_balance ~expected:amount ~ledger_name:"ledger_with_a" ~ticket_id
      ~ledger:ledger_with_a ()
  in
  let () =
    test_balance ~address_name:"a" ~address:a ~expected:amount
      ~ledger_name:"ledger_with_a" ~ticket_id ~ledger:ledger_with_a ()
  in
  let () =
    test_deposit_and_transfer ~address_name:"a" ~address:a
      ~address_balance:amount ~ledger_name:"ledger_with_a" ~ticket_id
      ~ledger:ledger_with_a ()
  in
  ()

let test_simple_transfer () =
  let a = new_address () in
  let b = new_address () in
  let total_amount = amount_of_int 6 in
  let ticket_id = random_ticket_id () in
  let ledger_with_a = deposit a total_amount ticket_id initial in
  let ledger_with_a_and_b =
    transfer ~sender:a ~receiver:b ~amount:(amount_of_int 4) ~ticket_id
      ledger_with_a
  in
  let () =
    Alcotest.(check' bool)
      ~msg:
        "Result.is_ok (transfer ~sender:a ~receiver:b 4 ledger_with_a) = true"
      ~expected:true
      ~actual:
        (Result.is_ok
           (transfer ~sender:a ~receiver:b ~amount:(amount_of_int 4) ~ticket_id
              ledger_with_a))
  in
  let ledger_with_a_and_b = Result.get_ok ledger_with_a_and_b in

  let () =
    test_total_balance ~expected:total_amount ~ledger_name:"ledger_with_a"
      ~ticket_id ~ledger:ledger_with_a ()
  in
  let () =
    test_balance ~address_name:"a" ~address:a ~expected:(amount_of_int 2)
      ~ticket_id ~ledger_name:"ledger_with_a_and_b" ~ledger:ledger_with_a_and_b
      ()
  in
  let () =
    test_balance ~address_name:"b" ~address:b ~expected:(amount_of_int 4)
      ~ticket_id ~ledger_name:"ledger_with_a_and_b" ~ledger:ledger_with_a_and_b
      ()
  in
  let () =
    test_deposit_and_transfer ~address_name:"a" ~address:a
      ~address_balance:(amount_of_int 2) ~ledger_name:"ledger_with_a" ~ticket_id
      ~ledger:ledger_with_a_and_b ()
  in
  let () =
    test_deposit_and_transfer ~address_name:"b" ~address:b
      ~address_balance:(amount_of_int 4) ~ledger_name:"ledger_with_a" ~ticket_id
      ~ledger:ledger_with_a_and_b ()
  in
  ()

let test_simple_withdraw () =
  let a = new_address () in
  let b = new_address () in
  let c =
    Deku_tezos.Address.Implicit
      (new_address () |> Address.to_key_hash |> Option.get)
  in
  let total_amount = amount_of_int 10 in
  let ticket1 = random_ticket_id () in
  let ledger = deposit a total_amount ticket1 initial in
  let ledger =
    transfer ~sender:a ~receiver:b ~amount:(amount_of_int 4) ~ticket_id:ticket1
      ledger
  in
  let ledger = Result.get_ok ledger in
  let ledger =
    withdraw ~sender:b ~destination:c ~amount:(amount_of_int 4)
      ~ticket_id:ticket1 ledger
  in
  Alcotest.(check' bool)
    ~msg:
      "Result.is_ok (withdraw ~sender:b ~destination:c 4 ledger_with_a) = true"
    ~expected:true ~actual:(Result.is_ok ledger)

let test_bad_withdraw1 () =
  let msg = "Cannot withdraw a ticket that was transferred" in
  let a = new_address () in
  let b = new_address () in
  let c =
    Deku_tezos.Address.Implicit
      (new_address () |> Address.to_key_hash |> Option.get)
  in
  let total_amount = amount_of_int 10 in
  let ticket1 = random_ticket_id () in
  let ledger = deposit a total_amount ticket1 initial in
  let ledger =
    transfer ~sender:a ~receiver:b ~amount:(amount_of_int 4) ~ticket_id:ticket1
      ledger
  in
  let ledger = Result.get_ok ledger in
  let ledger =
    withdraw ~sender:a ~destination:c ~amount:(amount_of_int 10)
      ~ticket_id:ticket1 ledger
  in
  Alcotest.(check' bool) ~msg ~expected:true ~actual:(Result.is_error ledger)

let test_bad_withdraw2 () =
  let msg = "Cannot withdraw a ticket that you do not have" in
  let a = new_address () in
  let b = new_address () in
  let c =
    Deku_tezos.Address.Implicit
      (new_address () |> Address.to_key_hash |> Option.get)
  in
  let total_amount = amount_of_int 10 in
  let ticket1 = random_ticket_id () in
  let ticket2 = random_ticket_id () in
  let ledger = deposit a total_amount ticket1 initial in
  let ledger = deposit a total_amount ticket2 ledger in
  let ledger =
    transfer ~sender:a ~receiver:b ~amount:(amount_of_int 4) ~ticket_id:ticket1
      ledger
  in
  let ledger = Result.get_ok ledger in
  let ledger =
    withdraw ~sender:b ~destination:c ~amount:(amount_of_int 10)
      ~ticket_id:ticket2 ledger
  in
  Alcotest.(check' bool) ~msg ~expected:true ~actual:(Result.is_error ledger)

let test_withdraw_balance () =
  let msg = "Total balance goes down after withdraw (tickets are burned)" in
  let a = new_address () in
  let b = new_address () in
  let c =
    Deku_tezos.Address.Implicit
      (new_address () |> Address.to_key_hash |> Option.get)
  in
  let initial_amount = amount_of_int 100 in
  let total_withdrawn = amount_of_int 20 in
  let ticket1 = random_ticket_id () in
  let ledger = deposit a initial_amount ticket1 initial in
  let ledger =
    transfer ~sender:a ~receiver:b ~amount:(amount_of_int 40) ~ticket_id:ticket1
      ledger
  in
  let ledger = Result.get_ok ledger in
  let ledger =
    withdraw ~sender:a ~destination:c ~amount:total_withdrawn ~ticket_id:ticket1
      ledger
  in
  let () =
    Alcotest.(check' bool)
      ~msg:"You can withdraw tickets that you didn't transfer" ~expected:true
      ~actual:(Result.is_ok ledger)
  in
  let ledger, _ = Result.get_ok ledger in
  let total_amount = total_balance ~ticket_id:ticket1 ledger in
  Alcotest.(check' bool)
    ~msg ~expected:true
    ~actual:(Some total_amount = Amount.(initial_amount - total_withdrawn))

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
          test_case "simple_withdraw" `Quick test_simple_withdraw;
          test_case "bad_withdraw1" `Quick test_bad_withdraw1;
          test_case "bad_withdraw2" `Quick test_bad_withdraw2;
          test_case "withdraw_balance" `Quick test_withdraw_balance;
        ] );
    ]
