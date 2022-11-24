type storage = { set : string -> string -> unit; get : string -> string option }

type chain =
  ( External_vm_protocol.vm_server_message,
    External_vm_protocol.vm_client_message )
  External_process.t

let state = ref External_vm_protocol.State.empty

let start_chain_ipc ~named_pipe_path =
  let opened_chain =
    External_process.open_chain_pipes ~named_pipe_path
      ~to_yojson:
        (Data_encoding.Json.construct
           External_vm_protocol.vm_server_message_encoding)
      ~of_yojson:
        (Data_encoding.Json.destruct
           External_vm_protocol.vm_client_message_encoding)
  in
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
    (match chain.receive () with
    | Transaction { source; operation; tickets; operation_raw_hash; level = _ }
      ->
        transition ~storage ~source ~tickets ~operation ~operation_raw_hash
    | Noop_transaction -> Ok ()
    | _ -> Error "protocol not respected")
    |> Result.fold
         ~ok:(fun _ -> External_vm_protocol.Stop)
         ~error:(fun err -> External_vm_protocol.Error err)
    |> chain.send;
    runtime_loop transition
  in

  runtime_loop transition
