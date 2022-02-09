open Crypto
type t =
  | Implicit of Key_hash.t 
  | Originated of {
  contract: Contract_hash.t ;
  entrypoint: string option } [@@deriving (eq, ord, yojson)]
val encoding : t Data_encoding.t
val to_string : t -> string
val of_string : string -> t option