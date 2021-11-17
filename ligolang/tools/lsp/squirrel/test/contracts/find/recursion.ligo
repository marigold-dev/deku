recursive function sum (const n : int; const acc : int) : int is
  if n < 1 then acc else sum (n-1, acc+n)
