open Cmdliner

type params = { wallet : string; api_uri : Uri.t; content : string }
[@@deriving cmdliner]

val cmd : unit Cmd.t
