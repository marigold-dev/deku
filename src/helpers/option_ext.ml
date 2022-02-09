include Option
module Let_syntax = struct
  let some = Option.some
  [%%let
  "let.none",
    fun v f ->
      match v with
      | None -> f ()
      | Some v -> Some v]
  [%%let "let.some", Option.bind]
  [%%let "let.default", fun v f -> Option.value (f ()) ~default:v]
end
