include module type of Option
module Let_syntax : sig
  val some : 'a -> 'a option
  [%%let ("let.none" : 'a option -> (unit -> 'a option) -> 'a option)]
  [%%let ("let.some" : 'a option -> ('a -> 'b option) -> 'b option)]
  [%%let ("let.default" : 'a -> (unit -> 'a option) -> 'a)]
end
