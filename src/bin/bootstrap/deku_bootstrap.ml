open Deku_stdlib
open Deku_crypto
open Deku_concepts
open Deku_consensus
open Deku_gossip
open Deku_network
open Deku_storage

let sleep_time = 3.0

module Util = struct
  let ensure_folder folder =
    let%await exists = Lwt_unix.file_exists folder in
    if exists then
      let%await stat = Lwt_unix.stat folder in
      if stat.st_kind = Lwt_unix.S_DIR then Lwt.return ()
      else raise (Invalid_argument (folder ^ " is not a folder"))
    else Lwt_unix.mkdir folder 0o700

  let storage_file ~n =
    let cwd = Unix.getcwd () in
    let data_folder = Filename.concat cwd "data" in
    let folder = Filename.concat data_folder (Int.to_string n) in
    Filename.concat folder "storage.json"
end

let broadcast ~content network =
  let open Message in
  let _message, raw_message = Message.encode ~content in
  let (Raw_message { hash; raw_content }) = raw_message in
  let raw_expected_hash = Message_hash.to_b58 hash in
  Network.broadcast ~raw_expected_hash ~raw_content network

let produce identity consensus network =
  let (Consensus.Consensus { state; _ }) = consensus in
  let (State { current_level; current_block; _ }) = state in
  let level = Level.next current_level in
  let previous = current_block in
  let operations = [] in
  let block = Block.produce ~identity ~level ~previous ~operations in
  broadcast ~content:(Message.Content.block block) network;
  block

let sign identity block network =
  let signature = Block.sign ~identity block in
  broadcast ~content:(Message.Content.signature signature) network

let restart ~producer identities consensus network =
  let block = produce producer consensus network in
  List.iter (fun identity -> sign identity block network) identities

let bootstrap ~size =
  let%await storages =
    let files = List.init size (fun n -> Util.storage_file ~n) in
    Lwt_list.map_p (fun file -> Storage.read ~file) files
  in
  let identities =
    List.map
      (fun storage ->
        let secret = storage.Storage.secret in
        Identity.make secret)
      storages
  in
  let producer =
    let length = List.length identities in
    let index = Random.int32 (Int32.of_int length) in
    let () = Format.eprintf "producer: %ld\n%!" index in
    List.nth identities (Int32.to_int index)
  in
  let validators =
    let storage = List.nth storages 0 in
    storage.Storage.validators
  in
  let consensus =
    let validators = List.map fst validators in
    let validators = Validators.of_key_hash_list validators in
    Consensus.make ~identity:producer ~validators
  in
  let network = Network.connect ~nodes:validators in
  (* TODO: this is lame, but I'm lazy*)
  let%await () = Lwt_unix.sleep sleep_time in
  let () = restart ~producer identities consensus network in
  (* TODO: this is lame, but Lwt*)
  let%await () = Lwt_unix.sleep sleep_time in
  Lwt.return_unit

let generate ~base_uri ~base_port ~size =
  let secrets =
    List.init size (fun _n ->
        let ed25519 = Ed25519.Secret.generate () in
        Secret.Ed25519 ed25519)
  in
  let validators =
    List.mapi
      (fun n secret ->
        let key = Key.of_secret secret in
        let key_hash = Key_hash.of_key key in

        let port = base_port + n in
        let uri = Uri.of_string base_uri in
        let uri = Uri.with_port uri (Some port) in
        (key_hash, uri))
      secrets
  in
  let storages =
    List.map (fun secret -> Storage.make ~secret ~validators) secrets
  in

  let%await cwd = Lwt_unix.getcwd () in
  let data_folder = Filename.concat cwd "data" in
  let%await () = Util.ensure_folder data_folder in

  Lwt_list.iteri_p
    (fun n storage ->
      let folder = Filename.concat data_folder (Int.to_string n) in
      let%await () = Util.ensure_folder folder in

      let file = Filename.concat folder "storage.json" in
      Storage.write ~file storage)
    storages

let main () =
  let base_uri = ref "http://localhost" in
  let base_port = ref 4440 in
  let size = ref 4 in
  let kind = ref None in

  Arg.parse
    [
      ( "-s",
        Arg.Set_string base_uri,
        {|Base URI for validators ("http://localhost" by default)|} );
      ("-p", Arg.Set_int base_port, "Base PORT for validators (4440 by default)");
      ("-n", Arg.Set_int size, "Number of validators (4 by default)");
    ]
    (fun selected -> kind := Some selected)
    "Handle Deku communication. Runs forever.";

  match !kind with
  | Some "generate" ->
      generate ~base_uri:!base_uri ~base_port:!base_port ~size:!size
  | Some "bootstrap" -> bootstrap ~size:!size
  | Some _ | None -> failwith "deku-bootstrap <generate|bootstrap>"

(* let setup_log ?style_renderer level =
     Fmt_tty.setup_std_outputs ?style_renderer ();
     Logs.set_level (Some level);
     Logs.set_reporter (Logs_fmt.reporter ())

   let () = setup_log Logs.Debug *)
let () = Lwt_main.run (main ())
