open Deku_consensus
open Deku_concepts
open Deku_stdlib
open Deku_gossip

type config = { save_messages : bool; save_blocks : bool }

type indexer =
  | Indexer of {
      pool : (Caqti_lwt.connection, Caqti_error.t) Caqti_lwt.Pool.t;
      config : config;
    }

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

  let insert_message message timestamp =
    let (Message.Raw_message { hash; raw_content }) = message in
    ignore (message, hash);
    let hash = Message_hash.to_b58 hash in
    let params = (hash, timestamp, raw_content) in
    let query =
      (tup3 string float string ->. unit)
      @@ "insert into packets (hash, timestamp, packet) values (?, ?, ?)"
    in
    return_unit query params

  let insert_message ~message ~timestamp pool =
    Caqti_lwt.Pool.use (insert_message message timestamp) pool
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

let make ~uri ~config =
  let%await () = make_database ~uri in
  match Caqti_lwt.connect_pool uri with
  | Ok pool -> Lwt.return (Indexer { pool; config })
  | Error err -> failwith (Caqti_error.show err)

let save_block ~block (Indexer { pool; config }) =
  match config.save_blocks with
  | true ->
      let timestamp = Unix.gettimeofday () in
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
  | false -> ()

let save_message ~message (Indexer { pool; config }) =
  match config.save_messages with
  | true ->
      Lwt.async (fun () ->
          let timestamp = Unix.gettimeofday () in
          let%await result = Query.insert_message ~message ~timestamp pool in
          match result with
          | Ok () -> Lwt.return_unit
          | Error err ->
              (* TODO: how do we want to handle this? *)
              raise (Caqti_error.Exn err))
  | false -> ()
