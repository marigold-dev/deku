function simple (const i : int) : int is if i = 2 then 42 else 0

function annot (const i : int) : int is
  block {
    const x : int = 41
  } with 1 + (if (i = 2 : bool) then (x : int) else (-1 : int))

function shadow (const i : int) : int is
  block {
    var result : int := 23;
    if i = 2 then result := 42 
             else result := 0
  } with result

