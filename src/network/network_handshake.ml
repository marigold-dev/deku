open Deku_crypto
open Deku_concepts

module Challenge = struct
  type challenge = Challenge of { hash : BLAKE2b.t }
  type t = challenge

  include BLAKE2b.With_b58_and_encoding_and_yojson (struct
    let name = "Network_handshake.Challenge"
    let prefix = Deku_repr.Prefix.deku_handshake_challenge_hash
  end)

  let encoding =
    Data_encoding.conv
      (fun (Challenge { hash }) -> hash)
      (fun hash -> Challenge { hash })
      encoding

  let generate () =
    let bytes = Random.generate BLAKE2b.digest_size in
    let bytes = Cstruct.to_string bytes in
    let hash = BLAKE2b.hash bytes in
    Challenge { hash }
end

module Response = struct
  type response = Response of { key : Key.t; signature : Signature.t }
  type t = response

  let key response =
    let (Response { key; signature = _ }) = response in
    key

  let encoding =
    Data_encoding.conv
      (fun (Response { key; signature }) -> (key, signature))
      (fun (key, signature) -> Response { key; signature })
      Signature.key_encoding

  let answer ~identity challenge =
    let open Challenge in
    let (Challenge { hash }) = challenge in
    let key = Identity.key identity in
    let signature = Identity.sign ~hash identity in
    Response { key; signature }

  let verify ~challenge response =
    let open Challenge in
    let (Response { key; signature }) = response in
    let (Challenge { hash }) = challenge in
    Signature.verify key signature hash
end
