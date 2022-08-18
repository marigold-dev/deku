module N = N
module Uri = Uri_ext

[%%let ("let.await" : 'a Lwt.t -> ('a -> 'b Lwt.t) -> 'b Lwt.t)]

[%%let
("let.ok" : ('a, 'err) result -> ('a -> ('b, 'err) result) -> ('b, 'err) result)]

[%%let ("let.some" : 'a option -> ('a -> 'b option) -> 'b option)]

module Parallel = Parallel

(* FIXME: not sure if this is the right thing to do.  *)
module Yojson : sig
  include module type of Yojson

  module Safe : sig
    include module type of Safe

    val t_of_yojson : t -> t
    val yojson_of_t : t -> t
  end
end

module Trace : sig
  val dump : string -> unit
  (* val start : unit -> unit *)
  (* val trace : string -> unit *)
end
