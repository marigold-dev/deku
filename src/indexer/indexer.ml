open Eio
open Deku_consensus
open Deku_concepts
open Deku_stdlib
open Deku_gossip

type config = { save_messages : bool; save_blocks : bool }

type indexer =
  | Indexer of {
      pool : (Caqti_eio.connection, Caqti_error.t) Caqti_eio.Pool.t;
      config : config;
    }

type t = indexer

module Query = struct
  open Caqti_request.Infix
  open Caqti_type.Std

  let use q pool = Caqti_eio.Pool.use q pool |> Promise.await

  let return_unit query param (module C : Caqti_eio.CONNECTION) =
    C.exec query param

  let return_opt query param (module C : Caqti_eio.CONNECTION) =
    C.find_opt query param

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
    use (insert_block block timestamp) pool

  let insert_message message timestamp =
    let (Message.Network.Network_message { raw_header; raw_content }) =
      message
    in
    ignore (message, raw_header);
    let params = (raw_header, timestamp, raw_content) in
    let query =
      (tup3 string float string ->. unit)
      @@ "insert into packets (hash, timestamp, packet) values (?, ?, ?)"
    in
    return_unit query params

  let insert_message ~message ~timestamp pool =
    use (insert_message message timestamp) pool

  let find_block level =
    let level = level |> Level.to_n |> N.to_z |> Z.to_int64 in
    let query =
      (int64 ->! tup2 float string)
      @@ "select timestamp, block from blocks where level=? order by timestamp \
          limit 1"
    in
    return_opt query level

  let find_block ~level pool = use (find_block level) pool

  let find_block_by_hash block_hash =
    let hash = block_hash |> Block_hash.yojson_of_t |> Yojson.Safe.to_string in
    let query =
      (string ->! string)
      @@ "select block from blocks where hash=? order by timestamp limit 1"
    in
    return_opt query hash

  let find_block_by_hash ~block_hash pool =
    use (find_block_by_hash block_hash) pool
end

let make_database ~uri =
  let open Caqti_request.Infix in
  let open Caqti_type.Std in
  let blocks_table_query (module C : Caqti_eio.CONNECTION) =
    (unit ->. unit)
    @@ {| create table if not exists blocks (
            hash TEXT not null,
            level BIGINT not null primary key,
            timestamp DOUBLE not null,
            block TEXT not null
          )
       |}
    |> fun query -> C.exec query ()
  in
  let packets_table_query (module C : Caqti_eio.CONNECTION) =
    (unit ->. unit)
    @@ {| create table if not exists packets (
            hash TEXT not null,
            timestamp DOUBLE not null,
            packet TEXT not null
          )
        |}
    |> fun query -> C.exec query ()
  in
  let blocks_res =
    Caqti_eio.with_connection uri blocks_table_query |> Promise.await
  in
  let packets_res =
    Caqti_eio.with_connection uri packets_table_query |> Promise.await
  in
  match (blocks_res, packets_res) with
  | Ok _, Ok _ -> ()
  | Error err, _ -> failwith (Caqti_error.show err)
  | _, Error err -> failwith (Caqti_error.show err)

let make ~uri ~config =
  let () = make_database ~uri in
  match Caqti_eio.connect_pool uri with
  | Ok pool -> Indexer { pool; config }
  | Error err -> failwith (Caqti_error.show err)

let async_save_block ~sw ~block (Indexer { pool; config }) =
  let open Deku_consensus in
  let on_error exn =
    Logs.err (fun m ->
        m "database/sqlite: exception %s" (Printexc.to_string exn))
  in
  match config.save_blocks with
  | true ->
      let timestamp = Unix.gettimeofday () in
      Eio.Fiber.fork_sub ~sw ~on_error (fun _sw ->
          let result = Query.insert_block ~block ~timestamp pool in
          let (Block.Block { level; _ }) = block in
          match result with
          | Ok () ->
              Logs.info (fun m ->
                  m "database/sqlite: block at level %a saved" Level.pp level)
          | Error err ->
              (* TODO: how do we want to handle this? *)
              raise (Caqti_error.Exn err))
  | false -> ()

let save_block ~block (Indexer { pool; config }) =
  match config.save_blocks with
  | true ->
      let timestamp = Unix.gettimeofday () in
      let _ = Query.insert_block ~block ~timestamp pool in
      ()
  | false -> ()

(* TODO: use this function *)
let _save_message ~sw ~message (Indexer { pool; config }) =
  match config.save_messages with
  | true ->
      Fiber.fork ~sw (fun () ->
          let timestamp = Unix.gettimeofday () in
          let result = Query.insert_message ~message ~timestamp pool in
          match result with
          | Ok () -> ()
          | Error err ->
              (* TODO: how do we want to handle this? *)
              raise (Caqti_error.Exn err))
  | false -> ()

let find_block_by_level ~level (Indexer { pool; config = _ }) =
  let result = Query.find_block ~level pool in
  match result with
  | Ok None -> None
  | Ok (Some (_, block_str)) ->
      block_str |> Yojson.Safe.from_string |> Block.t_of_yojson |> Option.some
  | Error err ->
      Caqti_error.show err |> print_endline;
      None

let find_block_by_hash ~block_hash (Indexer { pool; config = _ }) =
  let result = Query.find_block_by_hash ~block_hash pool in
  match result with
  | Ok res ->
      res |> Option.map Yojson.Safe.from_string |> Option.map Block.t_of_yojson
  | Error _ -> None
