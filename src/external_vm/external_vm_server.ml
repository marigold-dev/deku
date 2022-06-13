type storage = {
  set : string -> Yojson.Safe.t -> unit;
  get : string -> Yojson.Safe.t option;
}

type chain =
  ( External_vm_protocol.vm_server_message,
    External_vm_protocol.vm_client_message )
  External_process.t

let state = ref External_vm_protocol.State.empty

let start_chain_ipc ~named_pipe_path =
  let opened_chain =
    External_process.open_chain_pipes ~named_pipe_path
      ~to_yojson:External_vm_protocol.vm_server_message_to_yojson
      ~of_yojson:External_vm_protocol.vm_client_message_of_yojson in
  opened_chain

let rec init_state (chain : chain) initial_state =
  let message = chain.receive () in
  match message with
  | Get_Initial_State ->
    chain.send (Init initial_state);
    init_state chain initial_state
  | Set_Initial_State vm_state -> vm_state
  | _ -> failwith "protocol not respected"
(* initialize the state of the *)

let set (chain : chain) key value =
  chain.send (Set { key; value });
  (* TODO: need to check if it fails or not ? *)
  state := External_vm_protocol.State.set key value !state

let get key = External_vm_protocol.State.get key !state

let main ~named_pipe_path initial_state transition =
  let chain = start_chain_ipc ~named_pipe_path in
  state := init_state chain initial_state;
  let storage = { set = set chain; get } in

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
        transition storage sender tx_hash operation
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

  runtime_loop transition
