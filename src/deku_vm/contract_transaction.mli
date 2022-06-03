open External_vm.External_vm_server

type t =
  | Contract_invocation  of {
      to_invoke : Contract_address.t;
      argument : Contract_vm.Invocation_payload.t;
    }
  | Contract_origination of Contract_vm.Origination_payload.t
[@@deriving yojson]

val transaction :
  storage ->
  Crypto.Key_hash.t ->
  Crypto.BLAKE2B.t ->
  t ->
  (unit, [> `Invocation_error  of string | `Origination_error of string]) result
