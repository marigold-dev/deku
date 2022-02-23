open Crypto

(* TODO: operation here means Manager_operation *)

type transaction = {
  amount : Tez.t;
  destination : Address.t;
  entrypoint : string;
  (* WARNING: not lazy, only use with trusted communication *)
  value : Michelson.t;
}

type content = Transaction of transaction
type t = {
  source : Key_hash.t;
  fee : Tez.t;
  counter : Z.t;
  content : content;
  gas_limit : Gas.integral;
  storage_limit : Z.t;
}

(* TODO: better API for this *)
val forge :
  secret:Secret.t -> branch:Block_hash.t -> operations:t list -> string
