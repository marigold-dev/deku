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

let apply_operation key signature operation protocol =
  let (Protocol { included_operations; ledger }) = protocol in
  match
    Operation.verify key signature operation
    && not (Included_operation_set.mem operation included_operations)
  with
  | true ->
      let open Operation in
      let included_operations =
        Included_operation_set.add operation included_operations
      in
      let (Operation { hash = _; level = _; nonce = _; source; data }) =
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

let apply_operations operations protocol =
  List.fold_left
    (fun protocol (key, signature, operation) ->
      match apply_operation key signature operation protocol with
      | Some protocol -> protocol
      | None -> protocol)
    protocol operations

let clean ~current protocol =
  let (Protocol { included_operations; ledger }) = protocol in
  let included_operations =
    Included_operation_set.drop ~current included_operations
  in
  Protocol { included_operations; ledger }

let apply ~current ~operations protocol =
  let protocol = apply_operations operations protocol in
  clean ~current protocol
