open Helpers
open Tezos
open Http

let _z_integral_to_yojson, z_integral_of_yojson =
  Yojson_ext.with_data_encoding Gas.z_integral_encoding
type gas_z_integral = Gas.integral
let gas_z_integral_of_yojson = z_integral_of_yojson

type response = {
  hard_gas_limit_per_operation : gas_z_integral;
  hard_gas_limit_per_block : gas_z_integral;
  hard_storage_limit_per_operation : Z.t;
  cost_per_byte : Tez.t;
}
[@@deriving of_yojson { strict = false }]

let path ~chain ~block_hash =
  Format.sprintf "/chains/%s/blocks/%s/context/constants" chain block_hash

let execute ~node_uri ~chain ~block_hash =
  let chain =
    match chain with
    | Some chain -> Chain_id.to_string chain
    | None -> "main" in

  let block_hash =
    match block_hash with
    | Some block_hash -> Block_hash.to_string block_hash
    (* TODO: we could also query by height *)
    | None -> "head" in

  let path = path ~chain ~block_hash in
  http_get ~node_uri ~path ~of_yojson:response_of_yojson
