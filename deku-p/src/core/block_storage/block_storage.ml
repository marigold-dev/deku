open Eio
open Deku_consensus

type storage = Storage of { pool : Query.pool }
type t = storage

let or_fail p =
  match p with
  | Ok value -> value
  | Error err -> raise (Caqti_error.Exn (err :> Caqti_error.t))

let await_or_fail p = or_fail (Promise.await p)

let with_connection uri f =
  let connection = await_or_fail (Caqti_eio.connect uri) in
  Fun.protect ~finally:(fun () ->
      let (module Db) = connection in
      Promise.await (Db.disconnect ()))
  @@ fun () -> f connection

let prepare_database ~uri =
  with_connection uri @@ fun connection ->
  await_or_fail (Query.create_blocks_table () connection);
  await_or_fail (Query.create_packets_table () connection);
  await_or_fail (Query.create_block_and_votes_table () connection)

let make ~uri =
  let () = prepare_database ~uri in
  let pool = or_fail (Caqti_eio.connect_pool uri) in
  Storage { pool }

let save_block ~block storage =
  let (Storage { pool }) = storage in
  let timestamp = Unix.gettimeofday () |> Timestamp.of_float in
  await_or_fail (Query.insert_block ~block ~timestamp pool)

let save_block_and_votes ~level ~network storage =
  let (Storage { pool }) = storage in
  let timestamp = Unix.gettimeofday () |> Timestamp.of_float in
  await_or_fail (Query.insert_block_and_votes ~level ~network ~timestamp pool)

let save_message ~message storage =
  let (Storage { pool }) = storage in
  let timestamp = Unix.gettimeofday () |> Timestamp.of_float in
  await_or_fail (Query.insert_message ~message ~timestamp pool)

let find_block_by_level ~level storage =
  let (Storage { pool }) = storage in
  await_or_fail (Query.find_block_by_level ~level pool)

let find_block_by_hash ~block_hash storage =
  let (Storage { pool }) = storage in
  await_or_fail (Query.find_block_by_hash ~hash:block_hash pool)

let find_block_and_votes_by_level ~level storage =
  let (Storage { pool }) = storage in
  await_or_fail (Query.find_block_and_votes_by_level ~level pool)
