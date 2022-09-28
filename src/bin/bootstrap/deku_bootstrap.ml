open Deku_stdlib
open Deku_crypto
open Deku_concepts
open Deku_consensus
open Deku_gossip
open Deku_network
open Deku_storage
open Deku_chain

let sleep_time = 3.0

module Util = struct
  let ensure_folder ~env folder =
    let cwd = Eio.Stdenv.cwd env in
    match
      let folder = Filename.concat (Unix.getcwd ()) folder in

      IO.file_exists folder
    with
    | true -> ()
    | false ->
        let folder = Eio.Path.(cwd / folder) in
        Eio.Path.mkdir ~perm:0o755 folder

  let data_folders ~n =
    let data_folder = "data" in
    Filename.concat data_folder (Int.to_string n)
end

let broadcast ~content network =
  let open Message in
  let _message, raw_message = Message.encode ~content in
  let (Raw_message { hash; raw_content }) = raw_message in
  let raw_expected_hash = Message_hash.to_b58 hash in
  Network_manager.broadcast ~raw_expected_hash ~raw_content network

let produce identity consensus network =
  let (Block { hash = current_block; level = current_level; _ }) =
    Deku_consensus.Consensus.trusted_block consensus
  in
  let level = Level.next current_level in
  let previous = current_block in
  let operations = [] in
  let () =
    let level = Level.to_n level in
    let level = N.to_z level in
    Format.eprintf "produced: %a\n%!" Z.pp_print level
  in
  let block = Block.produce ~identity ~level ~previous ~operations in
  broadcast ~content:(Message.Content.block block) network;
  block

let sign identity block network =
  let vote = Block.sign ~identity block in
  let (Block { level; _ }) = block in
  let content = Message.Content.vote ~level ~vote in
  broadcast ~content network

let restart ~producer identities consensus network =
  let block = produce producer consensus network in
  List.iter (fun identity -> sign identity block network) identities

let bootstrap ~sw ~env ~size ~folder =
  let net = Eio.Stdenv.net env in
  let clock = Eio.Stdenv.clock env in
  let storages =
    let files = List.init size (fun n -> Util.data_folders ~n) in
    Eio.Fiber.List.map (fun folder -> Storage.Config.read ~env ~folder) files
  in
  let identities =
    List.map
      (fun storage ->
        let secret = storage.Storage.Config.secret in
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
    storage.Storage.Config.validators
  in
  let nodes =
    let storage = List.nth storages 0 in
    storage.Storage.Config.nodes
  in
  let chain = Storage.Chain.read ~env ~folder in
  let (Chain { consensus; _ }) =
    match chain with
    | Some chain -> chain
    | None -> Chain.make ~identity:producer ~validators
  in
  let network = Network_manager.make () in
  let () =
    Eio.Fiber.fork ~sw @@ fun () ->
    Network_manager.connect ~net ~clock
      ~on_request:(fun ~connection:_ ~raw_expected_hash:_ ~raw_content:_ -> ())
      ~on_message:(fun ~raw_expected_hash:_ ~raw_content:_ -> ())
      ~nodes network
  in
  (* TODO: this is lame, but I'm lazy *)
  let clock = Eio.Stdenv.clock env in
  let () = Eio.Time.sleep clock sleep_time in
  let () = restart ~producer identities consensus network in
  (* TODO: this is lame, but Lwt *)
  Eio.Time.sleep clock sleep_time

let generate ~env ~base_uri ~base_port ~size =
  let secrets =
    List.init size (fun _n ->
        let ed25519 = Ed25519.Secret.generate () in
        Secret.Ed25519 ed25519)
  in
  let validators =
    List.map
      (fun secret ->
        let key = Key.of_secret secret in
        let key_hash = Key_hash.of_key key in
        key_hash)
      secrets
  in
  let nodes =
    List.init size (fun n ->
        let port = base_port + n in
        (base_uri, port))
  in
  let storages =
    List.map
      (fun secret -> Storage.Config.make ~secret ~validators ~nodes)
      secrets
  in

  let data_folder = "data" in
  let () = Util.ensure_folder ~env data_folder in

  List.iteri
    (fun n storage ->
      let folder = Filename.concat data_folder (Int.to_string n) in
      let () = Util.ensure_folder ~env folder in
      Storage.Config.write ~env ~folder storage)
    storages

let main () =
  let base_uri = ref "localhost" in
  let base_port = ref 4440 in
  let size = ref 4 in
  let kind = ref None in
  let data_folder = ref "./data" in
  Arg.parse
    [
      ( "-s",
        Arg.Set_string base_uri,
        {|Base URI for validators ("localhost" by default)|} );
      ("-p", Arg.Set_int base_port, "Base PORT for validators (4440 by default)");
      ("-n", Arg.Set_int size, "Number of validators (4 by default)");
      ("-d", Arg.Set_string data_folder, " Data folder (./data by default)");
    ]
    (fun selected -> kind := Some selected)
    "Handle Deku communication. Runs forever.";

  Eio_main.run @@ fun env ->
  Eio.Switch.run @@ fun sw ->
  match !kind with
  | Some "generate" ->
      generate ~env ~base_uri:!base_uri ~base_port:!base_port ~size:!size
  | Some "bootstrap" -> bootstrap ~sw ~env ~size:!size ~folder:!data_folder
  | Some _ | None -> failwith "deku-bootstrap <generate|bootstrap>"

(* let setup_log ?style_renderer level =
     Fmt_tty.setup_std_outputs ?style_renderer ();
     Logs.set_level (Some level);
     Logs.set_reporter (Logs_fmt.reporter ())

   let () = setup_log Logs.Debug *)
let () = main ()
