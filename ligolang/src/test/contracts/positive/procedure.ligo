function my_assert (const p : bool) : unit is
  block { if p then skip else failwith("assert") }
  with unit

function main (const p : bool; const _s : int) : list(operation) * int is
  block {
    my_assert (p) ;
    const n : int = 4 ;
  } with ((nil : list(operation)), n)
