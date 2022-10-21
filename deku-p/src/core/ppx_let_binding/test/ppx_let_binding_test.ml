module M : sig
  [%%let ("let.some" : 'a option -> ('a -> 'b option) -> 'b option)]
end = struct
  [%%let "let.some", Option.bind]
end

open M

let option_two =
  let%some one = Some 1 in
  Some (one + 1)

let () = assert (option_two = Some 2)
