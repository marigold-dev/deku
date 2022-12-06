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

  let encoding =
    let open Data_encoding in
    conv
      (fun { key; signature; initial } -> (key, signature, initial))
      (fun (key, signature, initial) -> { key; signature; initial })
      (obj3 (req "key" Key.encoding)
         (req "signature" Signature.encoding)
         (req "initial" Operation.Initial.encoding))

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

  let encoding =
    let open Data_encoding in
    conv
      (fun {
             key;
             signature;
             hash;
             withdrawal_handles_hash;
             author;
             level;
             previous;
             payload;
             payload_hash;
             tezos_operations;
           } ->
        ( key,
          signature,
          hash,
          withdrawal_handles_hash,
          author,
          level,
          previous,
          payload,
          payload_hash,
          tezos_operations ))
      (fun ( key,
             signature,
             hash,
             withdrawal_handles_hash,
             author,
             level,
             previous,
             payload,
             payload_hash,
             tezos_operations ) ->
        {
          key;
          signature;
          hash;
          withdrawal_handles_hash;
          author;
          level;
          previous;
          payload;
          payload_hash;
          tezos_operations;
        })
      (tup10 Key.encoding Signature.encoding Block_hash.encoding
         BLAKE2b.encoding Key_hash.encoding Level.encoding Block_hash.encoding
         (list string) BLAKE2b.encoding
         (list Tezos_operation.encoding))
end
