module Let_syntax : sig
  val await : 'a -> 'a Lwt.t

  [%%let ("let.await" : 'a Lwt.t -> ('a -> 'b Lwt.t) -> 'b Lwt.t)]
end
