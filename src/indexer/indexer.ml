open Deku_consensus
open Deku_concepts
open Deku_stdlib
open Deku_network

type indexer = Indexer
type t = indexer

let pool_ref =
  ref None (* TODO: won't it be better to store it in the indexer ?*)

module Query = struct
  open Caqti_request.Infix
  open Caqti_type.Std

  let return_unit query param (module C : Caqti_lwt.CONNECTION) =
    C.exec query param

  let insert_block block =
    let (Block.Block { hash; level; _ }) = block in
    let hash = Block_hash.yojson_of_t hash |> Yojson.Safe.to_string in
    let block_json = Block.yojson_of_t block |> Yojson.Safe.to_string in
    let level = Level.to_n level |> N.to_z |> Z.to_int64 in
    let params = (hash, level, block_json) in
    let query =
      (tup3 string int64 string ->. unit)
      @@ "insert into blocks (hash, level, block) values (?, ?, ?)"
    in
    return_unit query params

  let insert_block ~block pool = Caqti_lwt.Pool.use (insert_block block) pool

  let insert_packet packet timestamp =
    let (Packet.Packet { hash; _ }) = packet in
    let hash = Packet_hash.yojson_of_t hash |> Yojson.Safe.to_string in
    let packet_json = Packet.yojson_of_t packet |> Yojson.Safe.to_string in
    let timestamp = Timestamp.to_float timestamp in
    let params = (hash, timestamp, packet_json) in
    let query =
      (tup3 string float string ->. unit)
      @@ "insert into packets (hash, timestamp, packet) values (?, ?, ?)"
    in
    return_unit query params

  let insert_packet ~packet ~timestamp pool =
    Caqti_lwt.Pool.use (insert_packet packet timestamp) pool
end

let pool () =
  match !pool_ref with
  | Some pool -> pool
  | None -> failwith "sqlite pool is not initialized"

let make ~uri =
  let pool = Caqti_lwt.connect_pool uri in
  let pool = match pool with Ok pool -> pool | Error _ -> failwith "lol" in
  pool_ref := Some pool;
  Indexer

let save_block ~block indexer =
  Lwt.async (fun () ->
      let pool = pool () in
      let%await result = Query.insert_block ~block pool in
      match result with
      | Ok _ ->
          print_endline "block saved";
          (*  TODO: replace with debug/trace log *)
          Lwt.return_unit
      | Error err ->
          print_endline (Caqti_error.show err);
          (* TODO: replace with error log *)
          Lwt.return_unit);
  indexer

let save_packet ~packet ~timestamp indexer =
  Lwt.async (fun () ->
      let pool = pool () in
      let packet = Yojson.Safe.from_string packet |> Packet.t_of_yojson in
      match Packet.verify packet with
      | false -> Lwt.return_unit
      | true -> (
          let%await result = Query.insert_packet ~packet ~timestamp pool in
          match result with
          | Ok _ -> Lwt.return_unit
          | Error err ->
              print_endline (Caqti_error.show err);
              Lwt.return_unit));
  indexer
