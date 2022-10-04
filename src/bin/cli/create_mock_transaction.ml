open Deku_crypto
open Deku_concepts
open Deku_protocol
open Deku_stdlib
open Deku_external_vm
open Cmdliner
open Common

type params = {
  secret : Ed25519.Secret.t;
  named_pipe_path : string; [@default "/tmp/vm_pipe"]
  content : string; [@pos 0]
  vm : string; [@post 1]
}
[@@deriving cmdliner]

(* Submits a parametric operation to the chain*)
let main { secret; named_pipe_path; content; vm } =
  let () = Stdlib.Random.self_init () in
  let secret = Secret.Ed25519 secret in
  let identity = Identity.make secret in
  let nonce = Utils.make_rnd_nonce () in
  let level = Level.zero in
  let operation = Operation.vm_transaction ~level ~nonce ~content ~identity in
  let (Operation.Operation { hash; _ }) = operation in
  let operation_raw_hash = Operation_hash.to_blake2b hash in

  (*starts the vm*)
  (*TODO: find a way to start the VM*)
  let prog, args =
    match String.split_on_char ' ' vm with
    | prog :: args -> (prog, args)
    | [] -> failwith "invalid vm parameter"
  in
  let args = List.append args [ named_pipe_path ] |> Array.of_list in
  print_endline prog;
  Array.iter print_endline args;
  let _pid = Unix.create_process prog args Unix.stdin Unix.stdout Unix.stderr in

  (* The rest of the code should worl :grimacing:*)
  let () = External_vm_client.start_vm_ipc ~named_pipe_path in
  let state = External_vm_client.get_initial_state () in
  let source = Identity.key_hash identity in
  let state =
    External_vm_client.apply_vm_operation_exn ~state ~source ~tickets:[]
      (Some (operation_raw_hash, content))
  in
  let json = External_vm_protocol.State.yojson_of_t state in
  print_endline (Yojson.Safe.to_string json);
  ()

let cmd =
  let term = Term.(const main $ params_cmdliner_term ()) in
  let info = Cmd.info "create-mock-transaction" in
  Cmd.v info term