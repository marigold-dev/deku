include Result
module Let_syntax = struct
  let ok = Result.ok
  [%%let "let.ok", Result.bind]
  [%%let
  "let.assert",
    fun (message, bool) f ->
      match bool with
      | true -> f ()
      | false -> Error message]

  (* When let%assert is not available *)
  let ensure (message, bool) =
    match bool with
    | true -> Lwt.return @@ Ok ()
    | false -> Lwt.return @@ Error message

  let ( let*? ) lwtx f =
    let open Lwt.Syntax in
    let* x = lwtx in
    match x with
    | Ok a -> f a
    | Error e -> Lwt.return @@ Error e
end
