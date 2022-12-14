open Deku_stdlib
open Deku_concepts
open Deku_consensus
open Deku_gossip

module Level : Rapper.CUSTOM with type t = Level.t = struct
  type t = Level.t

  let t =
    let encode level =
      level |> Level.to_n |> N.to_z |> Z.to_int64 |> Result.ok
    in
    let decode int64 =
      int64 |> Z.of_int64 |> N.of_z
      |> Option.to_result
           ~none:(Format.sprintf "invalid level: %s" (Int64.to_string int64))
      |> Result.map Level.of_n
    in
    Caqti_type.(custom ~encode ~decode int64)
end

module Block_hash : Rapper.CUSTOM with type t = Block_hash.t = struct
  type t = Block_hash.t

  let t =
    let encode block_hash = block_hash |> Block_hash.to_b58 |> Result.ok in
    let decode b58 =
      b58 |> Block_hash.of_b58
      |> Option.to_result
           ~none:(Format.sprintf "Cannot parse the block hash %s" b58)
    in
    Caqti_type.(custom ~encode ~decode string)
end

module Timestamp : Rapper.CUSTOM with type t = Timestamp.t = struct
  type t = Timestamp.t

  let t =
    let encode timestamp = timestamp |> Timestamp.to_float |> Result.ok in
    let decode float = Timestamp.of_float float |> Result.ok in
    Caqti_type.(custom ~encode ~decode float)
end

module Block : Rapper.CUSTOM with type t = Block.t = struct
  type t = Block.t

  let t =
    let encode block =
      (* TODO: in this commit we moved block storage onto the main thread,
         slowing us down considerably. We should explore moving it back off
         main thread. *)
      match Data_encoding.Binary.to_string Block.encoding block with
      | Ok binary -> Ok (Ezgzip.compress binary)
      | Error error ->
          Error
            (Format.asprintf "write error: %a"
               Data_encoding.Binary.pp_write_error error)
    in
    let decode compressed =
      match Ezgzip.decompress compressed with
      | Ok binary -> (
          match Data_encoding.Binary.of_string Block.encoding binary with
          | Ok network -> Ok network
          | Error error ->
              Error
                (Format.asprintf "%a" Data_encoding.Binary.pp_read_error error))
      | Error (`Gzip error) ->
          Error (Format.asprintf "%a" Ezgzip.pp_error error)
    in
    Caqti_type.(custom ~encode ~decode string)
end

module Message : Rapper.CUSTOM with type t = Message.Network.t = struct
  type t = Message.Network.t

  let t =
    let encode network =
      match Data_encoding.Binary.to_string Message.Network.encoding network with
      | Ok binary -> Ok (Ezgzip.compress binary)
      | Error error ->
          Error
            (Format.asprintf "write error: %a"
               Data_encoding.Binary.pp_write_error error)
    in
    let decode compressed =
      match Ezgzip.decompress compressed with
      | Ok binary -> (
          match
            Data_encoding.Binary.of_string Message.Network.encoding binary
          with
          | Ok network -> Ok network
          | Error error ->
              Error
                (Format.asprintf "%a" Data_encoding.Binary.pp_read_error error))
      | Error (`Gzip error) ->
          Error (Format.asprintf "%a" Ezgzip.pp_error error)
    in
    Caqti_type.(custom ~encode ~decode string)
end
