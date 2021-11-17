function multiply (const a : int; const b : int) : int is
  block {
    const result : int = a * b
  } with result

function add (const a : int; const b : int) : int is a + b

function main (const p : unit; const s : unit) : list (operation) * unit is
  ((nil : list (operation)), s)
