[%%let "let.await", Lwt.bind]
[%%let "let.some", Option.bind]
[%%let "let.ok", Result.bind]

[%%let
"let.assert",
  fun (message, bool) f ->
    match bool with true -> f () | false -> Error message]
