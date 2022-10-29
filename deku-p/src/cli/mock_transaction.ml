(* open Deku_concepts
   open Deku_protocol
   open Deku_stdlib
   open Deku_external_vm
   open Cmdliner
   open Common

   type params = {
     wallet : string; [@pos 0] [@docv "wallet"]
     named_pipe_path : string; [@default "/tmp/vm_pipe"]
     content : string; [@pos 1] [@docv "transaction"]
     vm : string; [@pos 2] [@docv "vm"]
   }
   [@@deriving cmdliner]

   let log message =
     Format.printf "[";
     Format.printf "\027[38;5;6m%s\027[0m" "deku-cli";
     Format.printf "] ";
     Format.printf "%s\n%!" message

   (* Submits a parametric operation to the chain*)
   let main { wallet; named_pipe_path; content; vm } =
     Eio_main.run @@ fun env ->
     let () = Stdlib.Random.self_init () in
     let secret = Wallet.read ~env ~file:wallet |> Wallet.priv_key in
     let identity = Identity.make secret in
     let nonce = Utils.make_rnd_nonce () in
     let level = Level.zero in
     let operation =
       Operation.Signed.vm_transaction ~level ~nonce ~content ~identity
     in
     let (Signed_operation { initial = Initial_operation { hash; _ }; _ }) =
       operation
     in
     let operation_raw_hash = Operation_hash.to_blake2b hash in

     log (Format.sprintf "Starting VM process '%s'" vm);
     let prog, args =
       match String.split_on_char ' ' vm with
       | prog :: args ->
           (prog, prog :: args) (* We need to keep $0 in the args list *)
       | [] -> failwith "invalid vm parameter"
     in
     let args = args @ [ named_pipe_path ] |> Array.of_list in
     let _pid = Unix.create_process prog args Unix.stdin Unix.stdout Unix.stderr in
     let () = External_vm_client.start_vm_ipc ~named_pipe_path in
     log "Retrieving initial state from VM";
     let state = External_vm_client.get_initial_state () in
     let source = Identity.key_hash identity in
     log "Applying transaction";
     let state =
       External_vm_client.apply_vm_operation_exn ~level
         ~ledger_api:
           (object
              method take_tickets _ = assert false
              method deposit _ _ = assert false
           end)
         ~state ~source ~tickets:[]
         (Some (operation_raw_hash, content))
     in
     let json = External_vm_protocol.State.yojson_of_t state in
     External_vm_client.close_vm_ipc ();
     (* FIXME: better formatting? *)
     log
       (Format.sprintf "Transaction complete. Final VM state: %s"
          (Yojson.Safe.pretty_to_string json))

   let cmd =
     let term = Term.(const main $ params_cmdliner_term ()) in
     let info = Cmd.info "mock-transaction" in
     Cmd.v info term *)
