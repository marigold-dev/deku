let chain = ref None

let start_chain_ipc ~named_pipe_path =
  chain :=
    Some
      (External_process.open_chain_pipes ~named_pipe_path
         ~to_yojson:External_vm_protocol.vm_message_to_yojson
           ~of_yojson:(function yojson -> Ok (Fun.id yojson)))

let main initial_state transition =
  match !chain with
  | None ->
    failwith
      "You must initialize the external VM IPC before getting the initial state"
  | Some chain ->
    let _init = chain.receive () in
    if _init = `String "init" then
      chain.send (External_vm_protocol.Init initial_state);

    let rec runtime_loop transition =
      if chain.receive () <> `String "close" then (
        let sender =
          match chain.receive () with
          | `String sender -> Crypto.Key_hash.of_string sender
          | _ -> None in
        let tx_hash =
          match chain.receive () with
          | `String tx_hash -> Crypto.BLAKE2B.of_string tx_hash
          | _ -> None in
        let operation = chain.receive () in
        (match (sender, tx_hash) with
        | Some sender, Some tx_hash -> transition sender tx_hash operation
        | None, _ -> Error "sender is required"
        | _, None -> Error "hash is required")
        |> Result.fold
             ~ok:(fun _ -> External_vm_protocol.Stop)
             ~error:(fun err -> External_vm_protocol.Error err)
        |> chain.send;
        runtime_loop transition)
      else
        () in
    runtime_loop transition
