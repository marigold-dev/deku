let main (p, s : bool * unit) =
  let () : unit = assert p
  in ([] : operation list), s

let with_error (p, s: bool * unit) =
  let _ : unit = assert_with_error p "my custom error" in
  ([] : operation list), s

let some (o : unit option) =
  assert_some o

let some_with_error (o : unit option) =
  assert_some_with_error o "my custom error"