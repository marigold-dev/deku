open Receipt
open Deku_tezos

type protocol =
  | Protocol of {
      included_operations : Included_operation_set.t;
      included_tezos_operations : Tezos_operation_hash.Set.t;
      ledger : Ledger.t;
    }

type t = protocol

let initial =
  Protocol
    {
      included_operations = Included_operation_set.empty;
      included_tezos_operations = Tezos_operation_hash.Set.empty;
      ledger = Ledger.initial;
    }

let apply_operation ~current_level protocol operation =
  let open Operation in
  let (Protocol { included_operations; ledger; included_tezos_operations }) =
    protocol
  in
  let (Operation
        { key = _; signature = _; hash; level; nonce = _; source; content }) =
    operation
  in
  match
    (* TODO: check code through different lane *)
    (not (Included_operation_set.mem operation included_operations))
    && Operation.is_in_includable_window ~current_level ~operation_level:level
  with
  | true -> (
      let included_operations =
        Included_operation_set.add operation included_operations
      in
      match content with
      | Operation_transaction { receiver; amount } ->
          let sender = source in
          let ledger =
            match Ledger.transfer ~sender ~receiver amount ledger with
            | Some ledger -> ledger
            | None -> ledger
          in

          let receipt = Receipt { operation = hash } in
          Some
            ( Protocol { included_operations; included_tezos_operations; ledger },
              receipt )
      | Operation_noop -> None)
  | false -> None

let apply_tezos_operation protocol tezos_operation =
  let (Protocol { included_operations; included_tezos_operations; ledger }) =
    protocol
  in
  let Tezos_operation.{ hash; operations } = tezos_operation in
  match not (Tezos_operation_hash.Set.mem hash included_tezos_operations) with
  | true ->
      let included_tezos_operations =
        Tezos_operation_hash.Set.add hash included_tezos_operations
      in
      let protocol =
        Protocol { included_operations; included_tezos_operations; ledger }
      in
      List.fold_left
        (fun protocol tezos_operation ->
          match tezos_operation with
          | Tezos_operation.Deposit _deposit ->
              print_endline "todo: handle the deposit in the ticket ledger";
              protocol)
        protocol operations
  | false -> protocol

let apply_tezos_operations tezos_operations protocol =
  List.fold_left apply_tezos_operation protocol tezos_operations

let parse_operation operation =
  match
    let json = Yojson.Safe.from_string operation in
    Operation.t_of_yojson json
  with
  | operation -> Some operation
  | exception _exn -> (* TODO: print exception *) None

let apply_payload ~parallel ~current_level payload protocol =
  let operations = parallel parse_operation payload in
  List.fold_left
    (fun (protocol, rev_receipts) operation ->
      match apply_operation ~current_level protocol operation with
      | Some (protocol, receipt) -> (protocol, receipt :: rev_receipts)
      | None -> (protocol, rev_receipts)
      | exception _exn -> (* TODO: print exception *) (protocol, rev_receipts))
    (protocol, []) operations

let clean ~current_level protocol =
  let (Protocol { included_operations; included_tezos_operations; ledger }) =
    protocol
  in
  let included_operations =
    Included_operation_set.drop ~current_level included_operations
  in
  Protocol { included_operations; included_tezos_operations; ledger }

let apply ~parallel ~current_level ~payload ~tezos_operations protocol =
  let protocol, receipts =
    apply_payload ~current_level ~parallel payload protocol
  in
  let protocol = clean ~current_level protocol in
  (* TODO: how to clean the set of tezos operations in memory? *)
  let protocol = apply_tezos_operations tezos_operations protocol in
  (protocol, receipts)
