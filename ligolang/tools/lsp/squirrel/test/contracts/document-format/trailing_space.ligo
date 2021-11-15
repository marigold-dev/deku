function main (const a : bool; const b : bool) : int is 
block {
  var result : int := 27; 
  if a = b then result := 999 else result := 1
} with result
