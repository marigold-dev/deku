open Tezos
open Http

type response = Operation_hash.t [@@deriving yojson]

(* TODO: should I use ?async *)
let path = "/injection/operation"

let execute ~node_uri ~secret ~branch ~operations =
  let signed_forged_operation = Operation.forge ~secret ~branch ~operations in
  http_post ~node_uri ~path ~of_yojson:response_of_yojson
    ~data:(`String signed_forged_operation)
