open Deku_stdlib
open Deku_concepts
open Deku_consensus

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
      block |> Block.yojson_of_t |> Yojson.Safe.to_string |> Ezgzip.compress
      |> Result.ok
    in
    let decode json =
      try
        json |> Ezgzip.decompress
        |> Result.map Yojson.Safe.from_string
        |> Result.map Block.t_of_yojson
        |> Result.map_error (fun _err -> "cannot decompress block")
      with exn ->
        Error
          (Format.sprintf "cannot decode block from the database: %s"
             (Printexc.to_string exn))
    in
    Caqti_type.(custom ~encode ~decode string)
end
