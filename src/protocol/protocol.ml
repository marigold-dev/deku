open Receipt

type protocol =
  | Protocol of {
      included_operations : Included_operation_set.t;
      ledger : Ledger.t;
    }

type t = protocol

let initial =
  Protocol
    {
      included_operations = Included_operation_set.empty;
      ledger = Ledger.initial;
    }

let apply_operation ~current_level protocol operation =
  let open Operation in
  let (Protocol { included_operations; ledger }) = protocol in
  let (Operation
        { key = _; signature = _; hash; level; nonce = _; source; content }) =
    operation
  in
  match
    (* TODO: check code through different lane *)
    (not (Included_operation_set.mem operation included_operations))
    && Operation.is_in_includable_window ~current_level ~operation_level:level
  with
  | true ->
      let included_operations =
        Included_operation_set.add operation included_operations
      in

      let ledger =
        match content with
        | Operation_transaction { receiver; amount } -> (
            let sender = source in
            match Ledger.transfer ~sender ~receiver amount ledger with
            | Some ledger -> ledger
            | None -> ledger)
      in

      let receipt = Receipt { operation = hash } in
      Some (Protocol { included_operations; ledger }, receipt)
  | false -> None

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
  let (Protocol { included_operations; ledger }) = protocol in
  let included_operations =
    Included_operation_set.drop ~current_level included_operations
  in
  Protocol { included_operations; ledger }

let apply ~parallel ~current_level ~payload protocol =
  let protocol, receipts =
    apply_payload ~parallel ~current_level payload protocol
  in
  let protocol = clean ~current_level protocol in
  (protocol, receipts)
