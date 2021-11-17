let main (parameter, storage : int * unit) =
  if parameter < 100 then ([] : operation list), ()
  else
    (Tezos.failwith
       "The passed parameter is too large, consider passing a value less than 100"
     : operation list * unit)
