open Deku_crypto

type bootstrap_signal =
  | Bootstrap_signal of {
      bootstrap_key : Key.t;
      signature : Signature.t;
      next_author : Key_hash.t;
    }

and t = bootstrap_signal [@@deriving yojson]

exception Invalid_signature

let t_of_yojson json =
  let (Bootstrap_signal { bootstrap_key; signature; next_author }) =
    t_of_yojson json
  in
  (match
     let hash = BLAKE2b.hash (Key_hash.to_b58 next_author) in
     Signature.verify bootstrap_key signature hash
   with
  | true -> ()
  | false -> raise Invalid_signature);

  Bootstrap_signal { bootstrap_key; signature; next_author }
