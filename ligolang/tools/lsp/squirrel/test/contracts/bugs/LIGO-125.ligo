function main (const parameter : int; const storage : unit) is
block {
  const nop = (list [] : list (operation));
  const result = (nop, Unit);
  if parameter =/= 100 then
    failwith
        ("The passed parameter is too large, consider passing a value less than 100");
  else skip
} with result