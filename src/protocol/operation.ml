open Deku_stdlib
open Deku_crypto
open Deku_concepts

exception Invalid_signature
exception Invalid_source

type operation_content =
  | Operation_ticket_transfer of { receiver : Address.t; amount : Amount.t }
  | Operation_vm_transaction of {
      operation : string;
      tickets : Ticket.t list; [@opaque]
    }
  | Operation_noop

and operation =
  | Operation of {
      (* TODO: I don't like that operation carries signature *)
      key : Key.t;
      (* TODO: should we use verified signatures here and elsewhere? *)
      signature : Signature.t; [@opaque]
      hash : Operation_hash.t;
      level : Level.t;
      nonce : Nonce.t;
      source : Address.t;
      content : operation_content;
    }

and t = operation [@@deriving yojson, show]

let equal a b =
  let (Operation { hash = a; _ }) = a in
  let (Operation { hash = b; _ }) = b in
  Operation_hash.equal a b

let compare a b =
  let (Operation { hash = a; _ }) = a in
  let (Operation { hash = b; _ }) = b in
  Operation_hash.compare a b

module Repr = struct
  type operation_content =
    | Transaction of { receiver : Address.t; amount : Amount.t }
    | Vm_transaction of { operation : string; tickets : Ticket.t list }
    | Noop

  and operation = {
    level : Level.t;
    nonce : Nonce.t;
    source : Address.t;
    content : operation_content;
  }

  and operation_with_signature = {
    key : Key.t;
    signature : Signature.t;
    operation : operation;
  }
  [@@deriving yojson]

  let hash operation =
    let json = yojson_of_operation operation in
    let json = Yojson.Safe.to_string json in
    Operation_hash.hash json

  let t_of_yojson json =
    let { key; signature; operation } =
      operation_with_signature_of_yojson json
    in
    let { level; nonce; source; content } = operation in
    let content =
      match content with
      | Transaction { receiver; amount } ->
          Operation_ticket_transfer { receiver; amount }
      | Vm_transaction { operation; tickets } ->
          Operation_vm_transaction { operation; tickets }
      | Noop -> Operation_noop
    in
    (* TODO: serializing after deserializing *)
    let hash = hash operation in

    (match
       let source = Address.to_key_hash source in
       Key_hash.(equal source (of_key key))
     with
    | true -> ()
    | false -> raise Invalid_source);
    (match
       let hash = Operation_hash.to_blake2b hash in
       Signature.verify key signature hash
     with
    | true -> ()
    | false -> raise Invalid_signature);
    Operation { key; signature; hash; level; nonce; source; content }

  let yojson_of_t operation =
    let (Operation { key; signature; hash = _; level; nonce; source; content })
        =
      operation
    in
    let content =
      match content with
      | Operation_ticket_transfer { receiver; amount } ->
          Transaction { receiver; amount }
      | Operation_vm_transaction { operation : string; tickets : Ticket.t list }
        ->
          Vm_transaction { operation; tickets }
      | Operation_noop -> Noop
    in
    let operation = { level; nonce; source; content } in
    yojson_of_operation_with_signature { key; signature; operation }
end

let t_of_yojson = Repr.t_of_yojson
let yojson_of_t = Repr.yojson_of_t

let ticket_transfer ~identity ~level ~nonce ~receiver ~amount =
  let source = Address.of_key_hash (Identity.key_hash identity) in
  let hash =
    let open Repr in
    let content = Transaction { receiver; amount } in
    let operation = { level; nonce; source; content } in
    hash operation
  in
  let key = Identity.key identity in
  let signature =
    let hash = Operation_hash.to_blake2b hash in
    Identity.sign ~hash identity
  in
  let content = Operation_ticket_transfer { receiver; amount } in
  Operation { key; signature; hash; level; nonce; source; content }

let noop ~identity ~level ~nonce =
  let source = Address.of_key_hash (Identity.key_hash identity) in
  let hash =
    let open Repr in
    let content = Noop in
    let source = Address.of_key_hash (Identity.key_hash identity) in
    let operation = { level; nonce; source; content } in
    hash operation
  in
  let key = Identity.key identity in
  let signature =
    let hash = Operation_hash.to_blake2b hash in
    Identity.sign ~hash identity
  in

  Operation
    { key; signature; hash; level; nonce; source; content = Operation_noop }

(* TODO: This seems like a weird place to put this function *)
let is_in_includable_window ~current_level ~operation_level =
  let open Level in
  let open Deku_constants in
  let last_includable_block =
    let operation_level = to_n operation_level in
    of_n N.(operation_level + includable_operation_window)
  in
  (* limits for how many blocks we need to hold the operations *)
  last_includable_block > current_level
