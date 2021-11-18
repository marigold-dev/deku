function main (const i : int) : int is
  block {
    var result : int := 23;
    if i = 2 then result := 42 else result := 0
  } with result

function foo (const b : bool) : int is
  block {
    const x : int = 41
  } with 1 + (if b then x else main (x))
