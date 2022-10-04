open Deku_crypto

type params = {
  secret : Ed25519.Secret.t;
  named_pipe_path : string;
  content : string;
  vm : string;
}
[@@deriving cmdliner]

val cmd : unit Cmdliner.Cmd.t
