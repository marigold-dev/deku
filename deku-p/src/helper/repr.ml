(* FIXME: Copied from the api *)
open Deku_crypto
open Deku_protocol
open Deku_consensus
open Deku_concepts

module Signed_operation = struct
  type t = {
    key : Key.t;
    signature : Signature.t;
    initial : Operation.Initial.t;
  }
  [@@deriving yojson]

  let of_signed signed =
    let (Operation.Signed.Signed_operation { key; signature; initial }) =
      signed
    in
    { key; signature; initial }

  let to_signed repr =
    let { key; signature; initial } = repr in
    Operation.Signed.make_with_signature ~key ~signature ~initial
end

module Block = struct
  type t = {
    key : Key.t;
    signature : Signature.t;
    hash : Block_hash.t;
    withdrawal_handles_hash : BLAKE2b.t;
    author : Key_hash.t;
    level : Level.t;
    previous : Block_hash.t;
    payload : string list;
    payload_hash : BLAKE2b.t;
    tezos_operations : Tezos_operation.t list;
  }
  [@@deriving yojson]

  let of_block block =
    let (Block.Block
          {
            key : Key.t;
            signature : Signature.t;
            hash : Block_hash.t;
            withdrawal_handles_hash : BLAKE2b.t;
            author : Key_hash.t;
            level : Level.t;
            previous : Block_hash.t;
            payload : string;
            payload_hash : BLAKE2b.t;
            tezos_operations : Tezos_operation.t list;
          }) =
      block
    in
    let payload = Payload.decode ~payload in
    let (Payload.Payload payload) = payload in
    {
      key : Key.t;
      signature : Signature.t;
      hash : Block_hash.t;
      withdrawal_handles_hash : BLAKE2b.t;
      author : Key_hash.t;
      level : Level.t;
      previous : Block_hash.t;
      payload;
      payload_hash : BLAKE2b.t;
      tezos_operations : Tezos_operation.t list;
    }
end
