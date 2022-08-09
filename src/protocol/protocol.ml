open Receipt

type protocol =
  | Protocol of {
      included_operations : Included_operation_set.t;
      included_tezos_operations : Included_tezos_operation_set.t;
      ledger : Ledger.t;
    }

type t = protocol

let initial =
  Protocol
    {
      included_operations = Included_operation_set.empty;
      included_tezos_operations = Included_tezos_operation_set.empty;
      ledger = Ledger.initial;
    }

let apply_operation protocol operation =
  let (Protocol { included_operations; included_tezos_operations; ledger }) =
    protocol
  in
  match
    (* TODO: check code through different lane *)
    not (Included_operation_set.mem operation included_operations)
  with
  | true ->
      let open Operation in
      let included_operations =
        Included_operation_set.add operation included_operations
      in
      let (Operation
            {
              key = _;
              signature = _;
              hash;
              level = _;
              nonce = _;
              source;
              content;
            }) =
        operation
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
      Some
        ( Protocol { included_operations; included_tezos_operations; ledger },
          receipt )
  | false -> None

let apply_tezos_operation protocol tezos_operation =
  let (Protocol { included_operations; included_tezos_operations; ledger }) =
    protocol
  in
  let Tezos_operation.{ hash; operations } = tezos_operation in
  match
    not (Included_tezos_operation_set.mem hash included_tezos_operations)
  with
  | true ->
      let included_tezos_operations =
        Included_tezos_operation_set.add hash included_tezos_operations
      in
      List.fold_left
        (fun _protocol tezos_operation ->
          match tezos_operation with
          | Tezos_operation.Deposit _deposit ->
              print_endline "todo: handle the deposit in the ticket ledger";
              Protocol
                { included_operations; included_tezos_operations; ledger })
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

let apply_payload ~parallel payload protocol =
  let operations = parallel parse_operation payload in
  List.fold_left
    (fun (protocol, rev_receipts) operation ->
      match apply_operation protocol operation with
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
  let protocol, receipts = apply_payload ~parallel payload protocol in
  let protocol = clean ~current_level protocol in
  let protocol = apply_tezos_operations tezos_operations protocol in
  (protocol, receipts)
