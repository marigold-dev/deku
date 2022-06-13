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
      (match chain.receive () with
      | Transaction { source; tx_hash; op_hash = _; operation } ->
        transition storage source tx_hash operation
      | _ -> Error "protocol not respected")
      |> Result.fold
           ~ok:(fun _ -> External_vm_protocol.Stop)
           ~error:(fun err -> External_vm_protocol.Error err)
      |> chain.send;
      runtime_loop transition)
    else
      chain.send (External_vm_protocol.Error "control not received") in

  runtime_loop transition
