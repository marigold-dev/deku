type t

val empty : t
val with_body : (module Handlers.HANDLERS) -> t -> t
val without_body : (module Handlers.NO_BODY_HANDLERS) -> t -> t

val make_handler :
  env:Eio.Stdenv.t -> t -> 'a Piaf.Server.ctx -> Piaf.Response.t
