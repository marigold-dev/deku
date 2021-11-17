type storage = {fn : (int -> int) option; value : int}

type parameter = SetFunction of (int -> int) | CallFunction

let call (fn, value : (int -> int) option * int) =
  match fn with
    Some f -> f value
  | None -> (failwith "Lambda is not set" : int)

let main (p, s : parameter * storage) =
  let newStorage =
    match p with
      SetFunction fn -> {s with fn = Some fn}
    | CallFunction -> {s with value = call (s.fn, s.value)} in
  ([] : operation list), newStorage
