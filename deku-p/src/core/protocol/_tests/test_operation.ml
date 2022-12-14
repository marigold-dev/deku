open Deku_protocol
open Deku_concepts
open Deku_crypto
open Deku_stdlib
open Deku_ledger

let alice_secret =
  Secret.of_b58 "edsk3EYk77QNx9HM4YDh5rv5nzBL68z2YGtBUGXhkw3rMhB2eCNvHf"
  |> Option.get

let alice = Identity.make alice_secret

let bob =
  Identity.make
    (Secret.of_b58 "edsk4Qejwxwj7JD93B45gvhYHVfMzNjkBWRQDaYkdt5JcUWLT4VDkh"
    |> Option.get)

let ticket_id =
  let address =
    Deku_tezos.Contract_hash.of_b58 "KT1JQ5JQB4P1c8U8ACxfnodtZ4phDVMSDzgi"
    |> Option.get
  in
  let data = Bytes.of_string "" in
  Ticket_id.make (Tezos address) data

let test_serde_transaction () =
  let transaction =
    Operation.Signed.ticket_transfer ~identity:alice ~level:Level.zero
      ~nonce:(Nonce.of_n N.one)
      ~receiver:(Address.of_key_hash (Identity.key_hash bob))
      ~ticket_id ~amount:Amount.zero
  in
  let transaction_str () =
    transaction
    |> Data_encoding.Binary.to_string_exn Operation.Signed.encoding
    |> Data_encoding.Binary.of_string_exn Operation.Signed.encoding
    |> fun _ -> ()
  in
  Alcotest.(check unit) "to and from yojson" () (transaction_str ())

let run () =
  let open Alcotest in
  run "Operation" ~and_exit:false
    [ ("Repr", [ test_case "inverse property" `Quick test_serde_transaction ]) ]
