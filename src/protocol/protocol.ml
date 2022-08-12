type protocol =
  | Protocol of {
      included_operations : Included_operation_set.t;
      included_tezos_operations : Deku_tezos.Tezos_operation_hash.Set.t;
      ledger : Ledger.t;
      contract_storage : Contract_storage.t;
    }

type t = protocol

let initial =
  Protocol
    {
      included_operations = Included_operation_set.empty;
      included_tezos_operations = Deku_tezos.Tezos_operation_hash.Set.empty;
      ledger = Ledger.initial;
      contract_storage = Contract_storage.empty;
    }

let rec apply_operation_content ~operation_hash ~source ~level ~nonce
    contract_storage ledger operation_content =
  let open Operation in
  match operation_content with
  | Operation_transaction { receiver; ticket_id; amount } -> (
      let sender = source in
      (* TODO: create receipts in the ledger *)
      match Ledger.transfer ~sender ~receiver ~ticket_id ~amount ledger with
      | Ok ledger ->
          (* TODO: change ~operation to ~operation_hash *)
          ((ledger, contract_storage), Receipt.success ~operation:operation_hash)
      | Error error ->
          ( (ledger, contract_storage),
            Receipt.error ~operation:operation_hash ~error ))
  | Operation_contract contract_operation ->
      let sender = source in
      let  Deku_vm.apply ~sender ~operation_hash ~level ~nonce
        ~apply_operation_content ledger contract_storage contract_operation

let apply_operation protocol operation =
  let (Protocol
        {
          included_operations;
          included_tezos_operations;
          ledger;
          contract_storage;
        }) =
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
            { key = _; signature = _; hash; level; nonce; source; content }) =
        operation
      in
      let (ledger, contract_storage), receipt =
        apply_operation_content ~operation_hash:hash ~source ~level ~nonce
          contract_storage ledger content
      in
      Some
        ( Protocol
            {
              included_operations;
              included_tezos_operations;
              ledger;
              contract_storage;
            },
          receipt )
  | false -> None

let apply_tezos_operation protocol tezos_operation =
  let (Protocol
        {
          included_operations;
          included_tezos_operations;
          ledger;
          contract_storage;
        }) =
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
        Protocol
          {
            included_operations;
            included_tezos_operations;
            ledger;
            contract_storage;
          }
      in
      List.fold_left
        (fun protocol tezos_operation ->
          match tezos_operation with
          | Tezos_operation.Deposit { destination; amount; ticket } ->
              let (Protocol
                    {
                      ledger;
                      included_operations;
                      included_tezos_operations;
                      contract_storage;
                    }) =
                protocol
              in
              let ticket_id =
                Ticket_id.from_tezos_ticket ticket |> Result.get_ok
              in
              let destination = Address.of_key_hash destination in
              let ledger = Ledger.deposit destination amount ticket_id ledger in
              Protocol
                {
                  ledger;
                  included_operations;
                  included_tezos_operations;
                  contract_storage;
                })
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
  let (Protocol
        {
          included_operations;
          included_tezos_operations;
          ledger;
          contract_storage;
        }) =
    protocol
  in
  let included_operations =
    Included_operation_set.drop ~current_level included_operations
  in
  Protocol
    { included_operations; included_tezos_operations; ledger; contract_storage }

let apply ~parallel ~current_level ~payload ~tezos_operations protocol =
  let protocol, receipts = apply_payload ~parallel payload protocol in
  let protocol = clean ~current_level protocol in
  (* TODO: how to clean the set of tezos operations in memory? *)
  let protocol = apply_tezos_operations tezos_operations protocol in
  (protocol, receipts)
