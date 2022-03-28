include module type of struct
  include Result
end
module Let_syntax : sig
  val ok : 'a -> ('a, 'b) result
  [%%let
  ("let.ok" : ('a, 'b) result -> ('a -> ('c, 'b) result) -> ('c, 'b) result)]
  [%%let
  ("let.assert" : 'a * bool -> (unit -> ('b, 'a) result) -> ('b, 'a) result)]

  val ensure : 'a * bool -> (unit, 'a) result Lwt.t
  val ( let*? ) :
    ('a, 'b) result Lwt.t ->
    ('a -> ('c, 'b) result Lwt.t) ->
    ('c, 'b) result Lwt.t
end
