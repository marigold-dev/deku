open Deku_stdlib
open Deku_crypto
open Deku_concepts
open Deku_consensus
open Deku_network
open Deku_storage

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

let produce identity consensus network =
  let (Consensus.Consensus { current_level; current_block; _ }) = consensus in
  let level = Level.next current_level in
  let previous = current_block in
  let operations = [] in
  let block = Block.produce ~identity ~level ~previous ~operations in
  let _network = Network.broadcast_block ~block network in
  block

let sign identity block network =
  let signature = Block.sign ~identity block in
  let _network = Network.broadcast_signature ~signature network in
  ()

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
  let nodes =
    let storage = List.nth storages 0 in
    storage.Storage.nodes
  in
  let validators =
    let validators = List.map Identity.key_hash identities in
    Validators.of_key_hash_list validators
  in
  let consensus = Consensus.make ~validators in
  let network = Network.make ~nodes in
  let () = restart ~producer identities consensus network in
  (* TODO: this is lame, but Lwt*)
  let%await () = Lwt_unix.sleep 3.0 in
  Lwt.return_unit

let make_database_uri ~node =
  let%await cwd = Lwt_unix.getcwd () in
  Filename.concat cwd (Printf.sprintf "data/%i/database.db" node)
  |> ( ^ ) "sqlite3:" |> Uri.of_string |> Lwt.return

let make_database ~database_uri =
  let open Caqti_request.Infix in
  let open Caqti_type.Std in
  let blocks_table_query (module C : Caqti_lwt.CONNECTION) =
    (unit ->. unit)
    @@ {| create table blocks (
            hash TEXT not null,
            level BIGINT not null,
            block TEXT not null
          )
       |}
    |> fun query -> C.exec query ()
  in
  let packets_table_query (module C : Caqti_lwt.CONNECTION) =
    (unit ->. unit)
    @@ {| create table packets (
            hash TEXT not null,
            timestamp DOUBLE not null,
            packet TEXT not null
          )
        |}
    |> fun query -> C.exec query ()
  in
  let%await blocks_res =
    Caqti_lwt.with_connection database_uri blocks_table_query
  in
  let%await packets_res =
    Caqti_lwt.with_connection database_uri packets_table_query
  in
  match (blocks_res, packets_res) with
  | Ok _, Ok _ -> Lwt.return_unit
  | Error err, _ -> failwith (Caqti_error.show err)
  | _, Error err -> failwith (Caqti_error.show err)

let generate ~base_uri ~base_port ~size =
  let secrets =
    List.init size (fun _n ->
        let ed25519 = Ed25519.Secret.generate () in
        Secret.Ed25519 ed25519)
  in
  let initial_validators =
    List.map
      (fun secret ->
        let key = Key.of_secret secret in
        Key_hash.of_key key)
      secrets
  in
  let nodes =
    List.init size (fun n ->
        let port = base_port + n in
        let uri = Uri.of_string base_uri in
        Uri.with_port uri (Some port))
  in
  let%await database_uris =
    List.init size (fun node -> node)
    |> Lwt_list.map_p (fun node -> make_database_uri ~node)
  in
  let storages =
    List.combine secrets database_uris
    |> List.map (fun (secret, database_uri) ->
           Storage.make ~secret ~initial_validators ~nodes ~database_uri)
  in

  let%await cwd = Lwt_unix.getcwd () in
  let data_folder = Filename.concat cwd "data" in
  let%await () = Util.ensure_folder data_folder in

  Lwt_list.iteri_p
    (fun n storage ->
      let folder = Filename.concat data_folder (Int.to_string n) in
      let%await () = Util.ensure_folder folder in

      let database_uri = Storage.(storage.database_uri) in
      let%await () = make_database ~database_uri in
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

let () = Lwt_main.run (main ())
