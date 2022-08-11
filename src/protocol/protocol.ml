open Receipt

type protocol =
  | Protocol of {
      included_operations : Included_operation_set.t;
      included_tezos_operations : Deku_tezos.Tezos_operation_hash.Set.t;
      ledger : Ledger.t;
    }

type t = protocol

let initial =
  Protocol
    {
      included_operations = Included_operation_set.empty;
      included_tezos_operations = Deku_tezos.Tezos_operation_hash.Set.empty;
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
      let ledger, receipts =
        match content with
        | Operation_transaction { receiver; ticket_id; amount } -> (
            let sender = source in
            match
              Ledger.transfer ~sender ~receiver ~ticket_id ~amount ledger
            with
            | Ok ledger -> ledger, []
            | Error _ -> ledger, [])
        | Tezos_withdraw { owner; amount; ticket } ->
            (let sender = source in
            match
              Ledger.withdraw ~sender ~destination:owner ~amount ~ticket_id:ticket ledger
            with
            | Ok (ledger, handle) -> ledger, [Receipt_tezos_withdraw handle]
            | Error _ -> ledger, [])
      in
      let receipts = Receipt_operation { operation = hash } :: receipts in
      Some
        ( Protocol { included_operations; included_tezos_operations; ledger },
          receipts )
  | false -> None

let apply_tezos_operation protocol tezos_operation =
  let (Protocol { included_operations; included_tezos_operations; ledger }) =
    protocol
  in
  let Tezos_operation.{ hash; operations } = tezos_operation in
  match
    not (Deku_tezos.Tezos_operation_hash.Set.mem hash included_tezos_operations)
  with
  | true ->
      let included_tezos_operations =
        Deku_tezos.Tezos_operation_hash.Set.add hash included_tezos_operations
      in
      let protocol =
        Protocol { included_operations; included_tezos_operations; ledger }
      in
      List.fold_left
        (fun protocol tezos_operation ->
          match tezos_operation with
          | Tezos_operation.Deposit { destination; amount; ticket } ->
              let (Protocol
                    { ledger; included_operations; included_tezos_operations })
                  =
                protocol
              in
              let ticket_id =
                Ticket_id.from_tezos_ticket ticket |> Result.get_ok
              in
              let destination = Address.of_key_hash destination in
              let ledger = Ledger.deposit destination amount ticket_id ledger in
              Protocol
                { ledger; included_operations; included_tezos_operations })
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
      | Some (protocol, receipts) -> (protocol, receipts @ rev_receipts)
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
  (* TODO: how to clean the set of tezos operations in memory? *)
  let protocol = apply_tezos_operations tezos_operations protocol in
  (protocol, receipts)
