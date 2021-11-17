function main (const parameter : int; const storage : unit) is
  if parameter < 100
  then ((list [] : list (operation)), Unit)
  else
    (failwith
       ("The passed parameter is too large, consider passing a value less than 100")
     : list (operation) * unit)
