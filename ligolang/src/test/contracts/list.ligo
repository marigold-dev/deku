// Test list type and related built-in functions in PascaLIGO

type foobar is list (int)

const fb : foobar = list [23; 42]

const fb2 : foobar = 144 # fb

const fb3 : foobar = cons (688, fb2)

const fb_head = List.head_opt (fb)

const fb_tail = List.tail_opt (fb)

function size_ (const m : foobar) : nat is size (m)

// function hdf (const m : foobar) : int is hd (m)

const bl : foobar = list [144; 51; 42; 120; 421]

function fold_op (const s : list (int)) : int is
  block {
    function aggregate (const prec: int; const cur: int) : int is prec+cur
  } with List.fold (aggregate, s, 10)

function iter_op (const s : list (int)) : int is
  block {
    var r : int := 0;
    function aggregate (const _i : int) : unit is
      block { skip (* r := r + 1 *) } with unit;
    List.iter (aggregate, s)
  } with r

function map_op (const s : list (int)) : list (int) is
  block {
    function increment (const i : int) : int is i+1
  } with List.map (increment, s)
