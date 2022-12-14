module Protocol_operation = Protocol_operation
module Protocol_receipt = Protocol_receipt
module Protocol_payload = Protocol_payload
open Protocol_receipt

type protocol =
  | Protocol of {
      included : Protocol_included_set.t;
      ledger : Protocol_ledger.t;
    }

and t = protocol

let encoding =
  let open Data_encoding in
  conv
    (fun (Protocol { included; ledger }) -> (included, ledger))
    (fun (included, ledger) -> Protocol { included; ledger })
    (tup2 Protocol_included_set.encoding Protocol_ledger.encoding)

let initial =
  Protocol
    { included = Protocol_included_set.empty; ledger = Protocol_ledger.initial }

let apply_internal_operation ~operation ledger =
  let open Protocol_operation.Internal in
  let ledger, outcome =
    match operation with
    | Internal_operation_register { sender; ticket_id } ->
        (* TODO: receipts*)
        let ledger, registered_code =
          Protocol_ledger.register ~owner:sender ~ticket:ticket_id ledger
        in
        (ledger, Internal_receipt_register { registered_code })
    (* TODO: source / sender mismatch naming *)
    | Internal_operation_transfer { sender; sender_code; receiver_code; amount }
      ->
        let ledger =
          Protocol_ledger.transfer ~owner:sender ~source:sender_code
            ~destination:receiver_code amount ledger
        in
        (ledger, Internal_receipt_transfer)
    | Internal_operation_noop { sender = _ } -> (ledger, Internal_receipt_noop)
  in
  (ledger, Internal_receipt_ok { operation; outcome })

let apply_internal_operation ~operation ledger =
  try apply_internal_operation ~operation ledger
  with exception_ -> (ledger, Internal_receipt_error { operation; exception_ })

let apply_signed_operation ~current_level ~raw_operation ~signed_operation
    protocol =
  let open Protocol_operation.Signed in
  let open Protocol_operation.Initial in
  let (Protocol { included; ledger }) = protocol in
  let (Signed_operation { key = _; signature = _; initial }) =
    signed_operation
  in
  let (Initial_operation { hash = _; nonce = _; level; source = _; operation })
      =
    initial
  in
  match
    (* TODO: check code through different lane *)
    (not (Protocol_included_set.mem raw_operation included))
    && is_in_includable_window ~current_level ~operation_level:level
  with
  | true ->
      (* TODO: check that incorrect operations are removed from the pool *)
      let included = Protocol_included_set.add raw_operation included in
      let ledger, internal_receipt =
        apply_internal_operation ~operation ledger
      in
      let protocol = Protocol { included; ledger } in
      (protocol, Signed_receipt_applied { signed_operation; internal_receipt })
  | false -> (protocol, Signed_receipt_duplicated { signed_operation })

let verify_raw_operation raw_operation =
  match raw_operation with
  | Some raw_operation -> (
      match Protocol_operation.Raw.verify raw_operation with
      | Some signed_operation -> Some (raw_operation, signed_operation)
      | None -> None)
  | None -> None

let apply_payload ~current_level ~payload protocol =
  (* TODO: what to do when operation is none? error? *)
  let payload = Seq.filter_map (fun x -> x) payload in
  let protocol, rev_receipts =
    Seq.fold_left
      (fun (protocol, rev_receipts) (raw_operation, signed_operation) ->
        let protocol, receipt =
          apply_signed_operation ~current_level ~raw_operation ~signed_operation
            protocol
        in
        (protocol, receipt :: rev_receipts))
      (protocol, []) payload
  in
  (protocol, List.rev rev_receipts)

let clean ~current_level protocol =
  let (Protocol { included; ledger }) = protocol in
  let included = Protocol_included_set.drop ~current_level included in
  Protocol { included; ledger }

type seq_map_p = { f : 'a 'b. ('a -> 'b) -> 'a Seq.t -> 'b Seq.t }
[@@ocaml.unboxed]

let apply ~seq_map_p ~current_level ~payload protocol =
  let payload = seq_map_p.f verify_raw_operation payload in
  let protocol, receipts = apply_payload ~current_level ~payload protocol in
  let protocol = clean ~current_level protocol in
  (protocol, Block_receipt { receipts })
