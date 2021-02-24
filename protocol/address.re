open Mirage_crypto_pk;

type key = Rsa.priv;
type t = Rsa.pub_; // TODO: is okay to have this public
let compare = compare;
let to_yojson = t => `String(Rsa.sexp_of_pub(t) |> Sexplib.Sexp.to_string);
let of_yojson =
  fun
  | `String(sexp) =>
    try(Ok(Sexplib.Sexp.of_string(sexp) |> Rsa.pub_of_sexp)) {
    | _ => Error("failed to parse")
    }
  | _ => Error("invalid type");
