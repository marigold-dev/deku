open Deku_crypto
open Cmdliner

type params = { secret : Ed25519.Secret.t; api_uri : Uri.t; content : string }
[@@deriving cmdliner]

val cmd : unit Cmd.t
