open Deku_consensus
open Deku_concepts
open Deku_stdlib
open Deku_network

type indexer =
  | Indexer of { pool : (Caqti_lwt.connection, Caqti_error.t) Caqti_lwt.Pool.t }

type t = indexer

module Query = struct
  open Caqti_request.Infix
  open Caqti_type.Std

  let return_unit query param (module C : Caqti_lwt.CONNECTION) =
    C.exec query param

  let insert_block block timestamp =
    let (Block.Block { hash; level; _ }) = block in
    let hash = Block_hash.yojson_of_t hash |> Yojson.Safe.to_string in
    let block_json = Block.yojson_of_t block |> Yojson.Safe.to_string in
    let timestamp = Timestamp.to_float timestamp in
    let level = Level.to_n level |> N.to_z |> Z.to_int64 in
    let params = (hash, level, timestamp, block_json) in
    let query =
      (tup4 string int64 float string ->. unit)
      @@ "insert into blocks (hash, level, timestamp, block) values (?, ?, ?, \
          ?)"
    in
    return_unit query params

  let insert_block ~block ~timestamp pool =
    Caqti_lwt.Pool.use (insert_block block timestamp) pool

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

let make_database ~uri =
  let open Caqti_request.Infix in
  let open Caqti_type.Std in
  let blocks_table_query (module C : Caqti_lwt.CONNECTION) =
    (unit ->. unit)
    @@ {| create table if not exists blocks (
            hash TEXT not null,
            level BIGINT not null,
            timestamp DOUBLE not null,
            block TEXT not null
          )
       |}
    |> fun query -> C.exec query ()
  in
  let packets_table_query (module C : Caqti_lwt.CONNECTION) =
    (unit ->. unit)
    @@ {| create table if not exists packets (
            hash TEXT not null,
            timestamp DOUBLE not null,
            packet TEXT not null
          )
        |}
    |> fun query -> C.exec query ()
  in
  let%await blocks_res = Caqti_lwt.with_connection uri blocks_table_query in
  let%await packets_res = Caqti_lwt.with_connection uri packets_table_query in
  match (blocks_res, packets_res) with
  | Ok _, Ok _ -> Lwt.return_unit
  | Error err, _ -> failwith (Caqti_error.show err)
  | _, Error err -> failwith (Caqti_error.show err)

let make ~uri =
  let%await () = make_database ~uri in
  match Caqti_lwt.connect_pool uri with
  | Ok pool -> Lwt.return (Indexer { pool })
  | Error err -> failwith (Caqti_error.show err)

let save_block ~block ~timestamp (Indexer { pool }) =
  Lwt.async (fun () ->
      let%await result = Query.insert_block ~block ~timestamp pool in
      match result with
      | Ok () ->
          print_endline "block saved";
          (*  TODO: replace with debug/trace log *)
          Lwt.return_unit
      | Error err ->
          (* TODO: how do we want to handle this? *)
          raise (Caqti_error.Exn err))

let save_packet ~packet ~timestamp (Indexer { pool }) =
  Lwt.async (fun () ->
      try
        let packet = Yojson.Safe.from_string packet |> Packet.t_of_yojson in
        let%await result = Query.insert_packet ~packet ~timestamp pool in
        match result with
        | Ok () -> Lwt.return_unit
        | Error _err ->
            (* TODO: how do we want to handle this? *)
            (* raise (Caqti_error.Exn err)*)
            Lwt.return_unit
      with _ -> Lwt.return_unit)
