module Libsecp256k1 = Libsecp256k1.External
open Libsecp256k1
let context =
  let c = Context.create () in
  let rand_value =
    Random.generate 32 |> Cstruct.to_bytes |> Bigstring.of_bytes in
  let randomized = Context.randomize c rand_value in
  if randomized then
    c
  else
    failwith "Secp256k1 context randomization failed. Aborting."
module Key = struct
  type t = Libsecp256k1.Key.public Libsecp256k1.Key.t
  let equal = Libsecp256k1.Key.equal
  let compare a b =
    Bigstring.compare (Libsecp256k1.Key.buffer a) (Libsecp256k1.Key.buffer b)
  let of_secret sk = Libsecp256k1.Key.neuterize_exn context sk
  let to_raw t = Libsecp256k1.Key.to_bytes context t |> Bigstring.to_string
  include Encoding_helpers.Make_b58 (struct
    type nonrec t = t
    let name = "Secp256k1.Public_key"
    let title = "A Secp256k1 public key"
    let size = Libsecp256k1.Key.compressed_pk_bytes
    let prefix = Base58.Prefix.secp256k1_public_key
    let to_raw = to_raw
    let of_raw string =
      string
      |> Bigstring.of_string
      |> Libsecp256k1.Key.read_pk context
      |> Result.to_option
  end)
end
module Key_hash = struct
  type t = BLAKE2B_20.t [@@deriving ord, eq]
  let of_key t = BLAKE2B_20.hash (Key.to_raw t)
  include Encoding_helpers.Make_b58 (struct
    type nonrec t = t
    let name = "Secp256k1.Public_key_hash"
    let title = "A Secp256k1 public key hash"
    let size = BLAKE2B_20.size
    let prefix = Base58.Prefix.secp256k1_public_key_hash
    let to_raw = BLAKE2B_20.to_raw_string
    let of_raw = BLAKE2B_20.of_raw_string
  end)
end
module Secret = struct
  type t = Libsecp256k1.Key.secret Libsecp256k1.Key.t
  let equal = Libsecp256k1.Key.equal
  let compare a b =
    Bigstring.compare (Libsecp256k1.Key.buffer a) (Libsecp256k1.Key.buffer b)
  include Encoding_helpers.Make_b58 (struct
    type nonrec t = t
    let name = "Secp256k1.Secret_key"
    let title = "A Secp256k1 secret key"
    let size = Libsecp256k1.Key.secret_bytes
    let prefix = Base58.Prefix.secp256k1_secret_key
    let to_raw t = Libsecp256k1.Key.to_bytes context t |> Bigstring.to_string
    let of_raw string =
      Libsecp256k1.Key.read_sk context (string |> Bigstring.of_string)
      |> Result.to_option
  end)
end
module Signature = struct
  type t = Sign.plain Sign.t
  let equal = Sign.equal
  let compare a b = Bigstring.compare (Sign.buffer a) (Sign.buffer b)
  let of_raw string =
    string |> Bigstring.of_string |> Sign.read context |> Result.to_option
  include Encoding_helpers.Make_b58 (struct
    type nonrec t = t
    let name = "Secp256k1"
    let title = "A Secp256k1 signature"
    let size = Sign.plain_bytes
    let prefix = Base58.Prefix.secp256k1_signature
    let to_raw t = Sign.to_bytes ~der:false context t |> Bigstring.to_string
    let of_raw = of_raw
  end)
  let zero = of_raw (String.make size '\x00') |> Option.get
end
let generate () =
  let seed = Random.generate 32 |> Cstruct.to_bytes in
  let sk = Libsecp256k1.Key.read_sk_exn context (Bigstring.of_bytes seed) in
  let pk = Libsecp256k1.Key.neuterize_exn context sk in
  (sk, pk)
let sign secret hash =
  BLAKE2B.to_raw_string hash
  |> Bigstring.of_string
  |> Sign.sign_exn context ~sk:secret
let verify public signature hash =
  Sign.verify_exn context ~pk:public
    ~msg:(Bigstring.of_string (BLAKE2B.to_raw_string hash))
    ~signature
