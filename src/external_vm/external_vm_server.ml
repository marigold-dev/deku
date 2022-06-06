let chain = ref None

let start_chain_ipc ~named_pipe_path =
  chain :=
    Some
      (External_process.open_chain_pipes ~named_pipe_path
         ~to_yojson:External_vm_protocol.vm_server_message_to_yojson
         ~of_yojson:External_vm_protocol.vm_client_message_of_yojson)

let main initial_state transition =
  match !chain with
  | None ->
    failwith
      "You must initialize the external VM IPC before getting the initial state"
  | Some chain ->
    let _init = chain.receive () in
    if _init = Get_Initial_State then (
      chain.send (External_vm_protocol.Init initial_state);
      let rec runtime_loop transition =
        if chain.receive () = Control then (
          let sender =
            match chain.receive () with
            | Source sender -> Crypto.Key_hash.of_string sender
            | _ -> None in
          let tx_hash =
            match chain.receive () with
            | Tx_hash tx_hash -> Crypto.BLAKE2B.of_string tx_hash
            | _ -> None in
          let operation =
            match chain.receive () with
            | Operation payload -> Some payload
            | _ -> None in
          (* let operation = chain.receive () in *)
          (match (sender, tx_hash, operation) with
          | Some sender, Some tx_hash, Some operation ->
            transition sender tx_hash operation
          | None, _, _ -> Error "sender is required"
          | _, None, _ -> Error "hash is required"
          | _, _, None -> Error "operation is required")
          |> Result.fold
               ~ok:(fun _ -> External_vm_protocol.Stop)
               ~error:(fun err -> External_vm_protocol.Error err)
          |> chain.send;
          runtime_loop transition)
        else
          chain.send (External_vm_protocol.Error "control not received") in

      runtime_loop transition)
