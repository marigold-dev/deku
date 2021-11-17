let main (ps : unit * unit) : operation list * unit =
  if true
  then (failwith "This contract always fails" : operation list * unit)
  else (failwith "This contract still always fails" : operation list * unit)
