let main (parameter, storage : bytes * int) : operation list * int =
  if parameter = 0xbc1ecb8e
  then ([] : operation list), storage + 1
  else
    if parameter = 0x36e44653
    then ([] : operation list), storage - 1
    else (failwith "Unknown entrypoint" : operation list * int)
