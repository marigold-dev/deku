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
type status =
  | Applied
  | Other
type t =
  | Transaction     of {
      source : Key_hash.t;
      status : status;
      internal_operations : internal_operation list;
    }
  | Non_transaction

val of_tezos_json : Yojson.Safe.t -> (t, string) result
