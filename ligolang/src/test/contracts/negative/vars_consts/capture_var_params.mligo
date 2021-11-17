let foo : int * int -> unit -> int =
  fun (p : int * int) ->
    match p with
      x[@var], y ->
        let bar : unit -> int = fun (_ : unit) -> x + y in
        bar

