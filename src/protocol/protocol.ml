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

let apply_operation protocol operation =
  let (Protocol { included_operations; ledger }) = protocol in
  match not (Included_operation_set.mem operation included_operations) with
  | true ->
      let open Operation in
      let included_operations =
        Included_operation_set.add operation included_operations
      in
      let (Operation
            {
              key = _;
              signature = _;
              hash = _;
              (* FIXME: We want to use this + includable_operation_window
                 to ensure operations have an expiration date. We currently
                 ignore this value. *)
              level = _;
              nonce = _;
              source;
              data;
            }) =
        operation
      in
      let ledger =
        match data with
        | Operation_transaction { receiver; amount } -> (
            let sender = source in
            match Ledger.transfer ~sender ~receiver amount ledger with
            | Some ledger -> ledger
            | None -> ledger)
      in
      Some (Protocol { included_operations; ledger })
  | false -> None

let parse_operation operation =
  match
    let json = Yojson.Safe.from_string operation in
    Operation.t_of_yojson json
  with
  | operation -> Some operation
  | exception _exn -> (* TODO: print exception *) None

let apply_operation protocol operation =
  match apply_operation protocol operation with
  | Some protocol -> protocol
  | None -> protocol
  | exception _exn -> (* TODO: print exception *) protocol

let apply_payload ~parallel payload protocol =
  let operations = parallel parse_operation payload in
  List.fold_left apply_operation protocol operations

let clean ~current_level protocol =
  let (Protocol { included_operations; ledger }) = protocol in
  let included_operations =
    Included_operation_set.drop ~current_level included_operations
  in
  Protocol { included_operations; ledger }

let apply ~parallel ~current_level ~payload protocol =
  let protocol = apply_payload ~parallel payload protocol in
  clean ~current_level protocol
