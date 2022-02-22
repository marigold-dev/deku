open Helpers
open Crypto

type t = string [@@deriving eq, ord]

include Encoding_helpers.Make_b58 (struct
  type nonrec t = t
  let name = "Chain_id"
  let title = "Network identifier"
  let size = 4
  let prefix = Base58.Prefix.chain_id
  let to_raw = Fun.id
  let of_raw s = if String.length s <> size then None else Some s
end)

let to_yojson, of_yojson =
  Yojson_ext.with_yojson_string "chain_id" to_string of_string
