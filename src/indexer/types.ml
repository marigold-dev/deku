open Deku_stdlib
open Deku_concepts

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
