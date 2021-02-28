open Mirage_crypto_pk;

type key = Rsa.priv;

let key_to_yojson = t =>
  `String(Rsa.sexp_of_priv(t) |> Sexplib.Sexp.to_string);
let key_of_yojson =
  fun
  | `String(sexp) =>
    try(Ok(Sexplib.Sexp.of_string(sexp) |> Rsa.priv_of_sexp)) {
    | _ => Error("failed to parse")
    }
  | _ => Error("invalid type");

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

let of_key = Rsa.pub_of_priv;
