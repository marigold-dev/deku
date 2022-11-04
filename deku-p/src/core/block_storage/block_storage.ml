open Eio
open Deku_consensus
open Deku_concepts
open Deku_gossip

type config = { save_messages : bool; save_blocks : bool }

type indexer =
  | Block_storage of {
      pool : (Caqti_eio.connection, Caqti_error.t) Caqti_eio.Pool.t;
      config : config;
    }

type t = indexer

module Query = struct
  let create_blocks_table =
    [%rapper
      execute
        {sql|
        CREATE TABLE IF NOT EXISTS blocks (
        hash TEXT not null,
        level BIGINT not null primary key,
        timestamp DOUBLE not null,
        block TEXT not null)
        |sql}]

  let create_packets_table =
    [%rapper
      execute
        {sql| CREATE TABLE IF NOT EXISTS packets (
              hash TEXT not null,
              timestamp DOUBLE not null,
              packet TEXT not null
              )|sql}]

  let insert_block =
    let open Types in
    [%rapper
      execute
        {sql|
        INSERT INTO blocks 
        (hash, level, timestamp, block) 
        VALUES
        (%Block_hash{block_hash}, %Level{level}, %Timestamp{timestamp}, %Block{block})    
        ON CONFLICT DO NOTHING
        |sql}]

  let insert_block ~block ~timestamp pool =
    let (Block.Block { hash = block_hash; level; _ }) = block in
    let block = Block.yojson_of_t block in
    Caqti_eio.Pool.use (insert_block ~block_hash ~level ~block ~timestamp) pool
    |> Promise.await

  let insert_message =
    let open Types in
    [%rapper
      execute
        {sql|
          INSERT INTO packets 
          (hash, timestamp, message)
          VALUES
          (%string{hash}, %Timestamp{timestamp}, %string{packet})    
          |sql}]

  let insert_message ~message ~timestamp pool =
    let (Message.Network.Network_message { raw_header = hash; raw_content }) =
      message
    in
    Caqti_eio.Pool.use
      (insert_message ~hash ~timestamp ~packet:raw_content)
      pool
    |> Promise.await

  let find_block_by_level =
    let open Types in
    [%rapper
      get_opt
        {sql|
          SELECT @Block{block}
          FROM blocks
          WHERE level = %Level{level}
        |sql}]

  let find_block_by_level ~level pool =
    Caqti_eio.Pool.use (find_block_by_level ~level) pool |> Promise.await

  let find_block_by_hash =
    let open Types in
    [%rapper
      get_opt
        {sql|
          SELECT @Block{block}
          FROM blocks
          WHERE hash = %Block_hash{hash}
          |sql}]

  let find_block_by_hash ~hash pool =
    Caqti_eio.Pool.use (find_block_by_hash ~hash) pool |> Promise.await
end

let make_database ~uri =
  let blocks_res =
    Caqti_eio.with_connection uri (Query.create_blocks_table ())
    |> Promise.await
  in
  let packets_res =
    Caqti_eio.with_connection uri (Query.create_packets_table ())
    |> Promise.await
  in
  match (blocks_res, packets_res) with
  | Ok _, Ok _ -> ()
  | Error err, _ -> failwith (Caqti_error.show err)
  | _, Error err -> failwith (Caqti_error.show err)

let make ~uri ~config =
  let () = make_database ~uri in
  match Caqti_eio.connect_pool uri with
  | Ok pool -> Block_storage { pool; config }
  | Error err -> failwith (Caqti_error.show err)

let async_save_block ~sw ~block (Block_storage { pool; config }) =
  let open Deku_consensus in
  let on_error exn =
    Logs.err (fun m ->
        m "database/sqlite: exception %s" (Printexc.to_string exn))
  in
  match config.save_blocks with
  | true ->
      Eio.Fiber.fork_sub ~sw ~on_error (fun _sw ->
          let timestamp = Unix.gettimeofday () |> Timestamp.of_float in
          let result = Query.insert_block ~block ~timestamp pool in
          match result with
          | Ok () ->
              let (Block.Block { level; _ }) = block in
              Logs.debug (fun m ->
                  m "database/sqlite: block at level %a saved" Level.pp level)
          | Error err ->
              (* TODO: how do we want to handle this? *)
              raise (Caqti_error.Exn err))
  | false -> ()

let save_block ~block (Block_storage { pool; config }) =
  match config.save_blocks with
  | true ->
      let timestamp = Unix.gettimeofday () |> Timestamp.of_float in
      let _ = Query.insert_block ~block ~timestamp pool in
      ()
  | false -> ()

(* TODO: use this function *)
let _save_message ~sw ~message (Block_storage { pool; config }) =
  let on_error exn =
    Logs.err (fun m ->
        m "database/sqlite: exception %s" (Printexc.to_string exn))
  in
  match config.save_messages with
  | true ->
      Fiber.fork_sub ~sw ~on_error (fun _sw ->
          let timestamp = Unix.gettimeofday () |> Timestamp.of_float in
          let result = Query.insert_message ~message ~timestamp pool in
          match result with
          | Ok () -> ()
          | Error err ->
              (* TODO: how do we want to handle this? *)
              raise (Caqti_error.Exn err))
  | false -> ()

let find_block_by_level ~level (Block_storage { pool; config = _ }) =
  let result = Query.find_block_by_level ~level pool in
  match result with
  | Ok block -> block
  | Error err ->
      Caqti_error.show err |> print_endline;
      None

let find_block_by_hash ~block_hash (Block_storage { pool; config = _ }) =
  let result = Query.find_block_by_hash ~hash:block_hash pool in
  match result with Ok res -> res | Error _ -> None
