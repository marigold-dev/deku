function main (const parameter : int; const storage : unit) is
  if parameter < 100
  then ((list [] : list (operation)), Unit)
  else (failwith ("PARAM_TOO_LARGE") : list (operation) * unit)
