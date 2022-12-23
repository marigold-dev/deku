module N = N
module Uri = Uri_ext
module Map = Map_ext
module Set = Set_ext
module IO = Io

[%%let
("let.ok" : ('a, 'err) result -> ('a -> ('b, 'err) result) -> ('b, 'err) result)]

[%%let ("let.some" : 'a option -> ('a -> 'b option) -> 'b option)]

module Parallel = Parallel
module List = List_ext

module Yojson : sig
  include module type of Yojson

  module Safe : sig
    include module type of Safe

    val t_of_yojson : t -> t
    val yojson_of_t : t -> t
  end
end