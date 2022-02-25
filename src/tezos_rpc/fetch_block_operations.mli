open Crypto
open Tezos

type parameters = {
  entrypoint : string;
  value : Michelson.t;
}
type internal_operation =
  | Internal_transaction     of {
      sender : Address.t;
      destination : Address.t;
      parameters : parameters option;
    }
  | Internal_non_transaction
type operation_status =
  | Applied
  | Other
type operation =
  | Transaction     of {
      source : Key_hash.t;
      status : operation_status;
      internal_operations : internal_operation list;
    }
  | Non_transaction
type block_operation = {
  (* TODO: should protocl also be here? *)
  (* TODO: should signature also be here? *)
  hash : Operation_hash.t;
  chain : Chain_id.t;
  branch : Block_hash.t;
  contents : operation list;
}
type response = block_operation list

val execute :
  node_uri:Uri.t ->
  chain:Chain_id.t option ->
  block_hash:Block_hash.t option ->
  (response, Error.error) result Lwt.t
