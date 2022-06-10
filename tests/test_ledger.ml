open Setup
open Helpers
open Crypto
open Core_deku
open Ledger

let () =
  describe "ledger" (fun { test; _ } ->
      let make_ticket ?ticketer ?data () =
        let open Tezos in
        let ticketer =
          match ticketer with
          | Some ticketer -> ticketer
          | None ->
            let random_hash =
              Random.generate 20
              |> Cstruct.to_string
              |> BLAKE2B_20.of_raw_string
              |> Option.get in
            Address.Originated { contract = random_hash; entrypoint = None }
        in
        let data =
          match data with
          | Some data -> data
          | None -> Random.generate 256 |> Cstruct.to_bytes in
        let open Ticket_id in
        { ticketer; data } in
      let make_address () =
        let _secret, _key, key_hash = Key_hash.make_ed25519 () in
        key_hash in
      let make_tezos_address () =
        let open Crypto in
        let open Tezos in
        let _key, address = Ed25519.generate () in
        let hash = Ed25519.Key_hash.of_key address in

        Address.Implicit (Ed25519 hash) in
      let setup_two () =
        let t1 = make_ticket () in
        let t2 = make_ticket () in
        let a = make_address () in
        let b = make_address () in
        let t =
          empty
          |> deposit a (Amount.of_int 100) t1
          |> deposit a (Amount.of_int 300) t2
          |> deposit b (Amount.of_int 200) t1
          |> deposit b (Amount.of_int 400) t2 in
        (t, (t1, t2), (a, b)) in
      let test name f =
        test name (fun { expect; _ } ->
            let expect_balance address ticket expected t =
              expect.equal (Amount.of_int expected) (balance address ticket t)
            in
            f expect expect_balance) in
      test "amount" (fun expect _ ->
          expect.equal
            (let open Amount in
            of_int 0 + of_int 5)
            (Amount.of_int 5);
          (expect.fn (fun () -> Amount.of_int (-1))).toThrowException
            (Invalid_argument "Negative amount"));
      test "balance" (fun _ expect_balance ->
          let t, (t1, t2), (a, b) = setup_two () in
          expect_balance a t1 100 t;
          expect_balance a t2 300 t;
          expect_balance b t1 200 t;
          expect_balance b t2 400 t;
          expect_balance (make_address ()) t1 0 t;
          expect_balance (make_address ()) t2 0 t;
          expect_balance a (make_ticket ()) 0 t;
          expect_balance b (make_ticket ()) 0 t);
      test "transfer" (fun expect expect_balance ->
          let t, (t1, t2), (a, b) = setup_two () in
          let c = make_address () in
          let t =
            transfer ~sender:(Address.of_key_hash a) ~destination:b
              (Amount.of_int 1) t1 t in
          (expect.result t).toBeOk ();
          let t = Result.get_ok t in
          expect_balance a t1 99 t;
          expect_balance a t2 300 t;
          expect_balance b t1 201 t;
          expect_balance b t2 400 t;
          expect_balance c t1 0 t;
          expect_balance c t2 0 t;
          let t =
            transfer ~sender:(Address.of_key_hash b) ~destination:a
              (Amount.of_int 3) t2 t in
          (expect.result t).toBeOk ();
          let t = Result.get_ok t in
          expect_balance a t1 99 t;
          expect_balance a t2 303 t;
          expect_balance b t1 201 t;
          expect_balance b t2 397 t;
          expect_balance c t1 0 t;
          expect_balance c t2 0 t;
          let t =
            transfer ~sender:(Address.of_key_hash b) ~destination:c
              (Amount.of_int 5) t2 t in
          (expect.result t).toBeOk ();
          let t = Result.get_ok t in
          expect_balance a t1 99 t;
          expect_balance a t2 303 t;
          expect_balance b t1 201 t;
          expect_balance b t2 392 t;
          expect_balance c t1 0 t;
          expect_balance c t2 5 t;
          let t =
            transfer ~sender:(Address.of_key_hash a) ~destination:c
              (Amount.of_int 99) t1 t in
          (expect.result t).toBeOk ();
          let t = Result.get_ok t in
          expect_balance a t1 0 t;
          expect_balance a t2 303 t;
          expect_balance b t1 201 t;
          expect_balance b t2 392 t;
          expect_balance c t1 99 t;
          expect_balance c t2 5 t;
          (let t =
             transfer ~sender:(Address.of_key_hash b) ~destination:c
               (Amount.of_int 202) t1 t in
           (expect.result t).toBeError ();
           expect.equal (Result.get_error t) `Insufficient_funds);
          (let d = make_address () in
           let t =
             transfer ~sender:(Address.of_key_hash d) ~destination:c
               (Amount.of_int 1) t2 t in
           (expect.result t).toBeError ();
           expect.equal (Result.get_error t) `Insufficient_funds);
          (let t3 = make_ticket () in
           let t =
             transfer ~sender:(Address.of_key_hash a) ~destination:b
               (Amount.of_int 1) t3 t in
           (expect.result t).toBeError ();
           expect.equal (Result.get_error t) `Insufficient_funds);
          ());
      test "deposit" (fun _ expect_balance ->
          let t, (t1, t2), (a, b) = setup_two () in
          let t = deposit a (Amount.of_int 123) t1 t in
          expect_balance a t1 223 t;
          expect_balance a t2 300 t;
          expect_balance b t1 200 t;
          expect_balance b t2 400 t;
          let t = deposit b (Amount.of_int 456) t2 t in
          expect_balance a t1 223 t;
          expect_balance a t2 300 t;
          expect_balance b t1 200 t;
          expect_balance b t2 856 t);
      test "withdraw" (fun expect expect_balance ->
          let t, (t1, t2), (a, b) = setup_two () in
          let destination = make_tezos_address () in
          let t = withdraw ~sender:a ~destination (Amount.of_int 10) t1 t in
          (expect.result t).toBeOk ();
          let t, handle = Result.get_ok t in
          expect_balance a t1 90 t;
          expect_balance a t2 300 t;
          expect_balance b t1 200 t;
          expect_balance b t2 400 t;
          expect.equal handle.id 0;
          expect.equal handle.owner destination;
          expect.equal handle.amount (Amount.of_int 10);
          let t = withdraw ~sender:b ~destination (Amount.of_int 9) t2 t in
          (expect.result t).toBeOk ();
          let t, handle = Result.get_ok t in
          expect_balance a t1 90 t;
          expect_balance a t2 300 t;
          expect_balance b t1 200 t;
          expect_balance b t2 391 t;
          expect.equal handle.id 1;
          expect.equal handle.owner destination;
          expect.equal handle.amount (Amount.of_int 9);
          let t = withdraw ~sender:a ~destination (Amount.of_int 8) t2 t in
          (expect.result t).toBeOk ();
          let t, handle = Result.get_ok t in
          expect_balance a t1 90 t;
          expect_balance a t2 292 t;
          expect_balance b t1 200 t;
          expect_balance b t2 391 t;
          expect.equal handle.id 2;
          expect.equal handle.owner destination;
          expect.equal handle.amount (Amount.of_int 8);
          (let t = withdraw ~sender:a ~destination (Amount.of_int 91) t1 t in
           (expect.result t).toBeError ());
          (let t = withdraw ~sender:b ~destination (Amount.of_int 203) t1 t in
           (expect.result t).toBeError ());
          (let c = make_address () in
           let t = withdraw ~sender:c ~destination (Amount.of_int 1) t1 t in
           (expect.result t).toBeError ());
          ());
      test "compare" (fun expect _ ->
          let t, (t1, _), (a, b) = setup_two () in
          (let t1' = make_ticket ~data:t1.data () in
           let t =
             transfer ~sender:(Address.of_key_hash a) ~destination:b
               (Amount.of_int 1) t1' t in
           (expect.result t).toBeError ();
           expect.equal (Result.get_error t) `Insufficient_funds);
          (let t1' = make_ticket ~ticketer:t1.ticketer () in
           let t =
             transfer ~sender:(Address.of_key_hash a) ~destination:b
               (Amount.of_int 1) t1' t in
           (expect.result t).toBeError ();
           expect.equal (Result.get_error t) `Insufficient_funds);
          ()))
