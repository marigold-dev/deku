// Test set iteration in PascaLIGO

function iter_op (const s : set (int)) : int is
  block {
    var r : int := 0;
    function aggregate (const _i : int) : unit is
      block {
        skip (* r := r + 1 *)
      } with unit;
    set_iter (aggregate, s)
  } with r // ALWAYS RETURNS 0

function fold_op (const s : set (int)) : int is
  block {
    function aggregate (const i : int; const j : int) : int is
      i + j
  } with set_fold (aggregate, s, 15)
