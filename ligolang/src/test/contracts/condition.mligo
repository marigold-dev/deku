// Test conditional in CameLIGO

let simple (i : int) = if i = 2 then 42 else 0

let annot (i : int) = if (i = 2 : bool) then (42 : int) else (0 : int)

let shadow (i : int) =
  let _result = 0 in
  if i = 2
  then let _result = 42 in _result
  else let _result = 0 in _result