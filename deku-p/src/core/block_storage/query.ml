open Deku_stdlib
open Deku_consensus
open Deku_gossip

type connection = (module Rapper_helper.CONNECTION)
type pool = ((module Rapper_helper.CONNECTION), Caqti_error.t) Caqti_eio.Pool.t
type 'a result_promise = ('a, Caqti_error.t) result Eio.Promise.t

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

let create_block_and_votes_table =
  [%rapper
    execute
      {sql|
      CREATE TABLE IF NOT EXISTS block_and_votes (
      level BIGINT not null primary key,
      timestamp DOUBLE not null,
      message TEXT not null)
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
      |sql}]

let insert_block_and_votes =
  let open Types in
  [%rapper
    execute
      {sql|
      INSERT INTO block_and_votes 
      (level, timestamp, message) 
      VALUES
      (%Level{level}, %Timestamp{timestamp}, %Message{network})
      |sql}]

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

let insert_block ~block ~timestamp pool =
  let (Block.Block { hash = block_hash; level; _ }) = block in
  let block =
    Parallel.parallel @@ fun () ->
    Data_encoding.Json.construct Block.encoding block
  in
  Caqti_eio.Pool.use (insert_block ~block_hash ~level ~block ~timestamp) pool

let insert_block_and_votes ~level ~network ~timestamp pool =
  Caqti_eio.Pool.use (insert_block_and_votes ~level ~network ~timestamp) pool

let insert_message ~message ~timestamp pool =
  let (Message.Network.Network_message { raw_header = hash; raw_content }) =
    message
  in
  Caqti_eio.Pool.use (insert_message ~hash ~timestamp ~packet:raw_content) pool

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
  Caqti_eio.Pool.use (find_block_by_level ~level) pool

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
  Caqti_eio.Pool.use (fun pool -> find_block_by_hash ~hash pool) pool

let find_block_and_votes_by_level =
  let open Types in
  [%rapper
    get_opt
      {sql|
        SELECT @Message{message}
        FROM block_and_votes
        WHERE level = %Level{level}
      |sql}]

let find_block_and_votes_by_level ~level pool =
  Caqti_eio.Pool.use (find_block_and_votes_by_level ~level) pool
