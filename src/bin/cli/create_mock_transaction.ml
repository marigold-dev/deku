open Deku_concepts
open Deku_protocol
open Deku_stdlib
open Deku_external_vm
open Cmdliner
open Common

type params = {
  wallet : string; [@pos 0]
  named_pipe_path : string; [@default "/tmp/vm_pipe"]
  content : string; [@pos 1]
  vm : string; [@post 2]
}
[@@deriving cmdliner]

(* Submits a parametric operation to the chain*)
let main { wallet; named_pipe_path; content; vm } =
  Eio_main.run @@ fun env ->
  let () = Stdlib.Random.self_init () in
  let secret = Wallet.read ~env ~file:wallet |> Wallet.priv_key in
  let identity = Identity.make secret in
  let nonce = Utils.make_rnd_nonce () in
  let level = Level.zero in
  let operation = Operation.vm_transaction ~level ~nonce ~content ~identity in
  let (Operation.Operation { hash; _ }) = operation in
  let operation_raw_hash = Operation_hash.to_blake2b hash in

  let prog, args =
    match String.split_on_char ' ' vm with
    | prog :: args -> (prog, prog :: args)  (* We need to keep $0 in the args list *)
    | [] -> failwith "invalid vm parameter"
  in
  let args = (args @ [named_pipe_path]) |> Array.of_list in
  (* TODO: see if fifo exist
  let named_pipe_path = "/tmp/deku_named_pipe_" ^ suffix in
   *)
  let _pid = Unix.create_process prog args Unix.stdin Unix.stdout Unix.stderr in
  let () = External_vm_client.start_vm_ipc ~named_pipe_path in
  let state = External_vm_client.get_initial_state () in
  let source = Identity.key_hash identity in
  let state =
    External_vm_client.apply_vm_operation_exn ~state ~source ~tickets:[]
      (Some (operation_raw_hash, content))
  in
  let json = External_vm_protocol.State.yojson_of_t state in
  External_vm_client.close_vm_ipc ();
  print_endline (Yojson.Safe.pretty_to_string json) (* FIXME: do better? *)

let cmd =
  let term = Term.(const main $ params_cmdliner_term ()) in
  let info = Cmd.info "create-mock-transaction" in
  Cmd.v info term
