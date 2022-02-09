include Result
module Let_syntax =
  struct
    let ok = Result.ok
    [%%let ((("let.ok")), Result.bind)]
    [%%let
      ((("let.assert")),
        (fun (message, bool) ->
           fun f ->
             match bool with
             | true -> f ()
             | false -> ((Error (message)))))]
  end