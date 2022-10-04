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
