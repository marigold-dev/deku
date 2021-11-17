// Test map type and related built-in functions in PascaLIGO

type foobar is map (int, int)

const empty_map : foobar = map []

const map1 : foobar = map [
  144 -> 23;
  51  -> 23;
  42  -> 23;
  120 -> 23;
  421 -> 23]

const map2 : foobar = map [23 -> 0; 42 -> 0]

function set_ (var n : int; var m : foobar) : foobar is block {
  m[23] := n
} with m

function add (var n : int ; var m : foobar) : foobar is set_(n,m)

function rm (var m : foobar) : foobar is block {
  remove 42 from map m
} with m

function patch_ (var m : foobar) : foobar is block {
  patch m with map [0 -> 5; 1 -> 6; 2 -> 7]
} with m

function patch_deep (var m : foobar * nat) : foobar * nat is
  block { patch m.0 with map [1 -> 9] } with m

function size_ (const m : foobar) : nat is Map.size (m)

function get (const m : foobar) : option (int) is m[42]

function mem (const k: int; const m: foobar) : bool is Map.mem (k, m)

function iter_op (const m : foobar) : unit is
  block {
    function aggregate (const i : int; const j : int) : unit is block
      { if i=j then skip else failwith ("fail") } with unit
  } with Map.iter (aggregate, m)

function map_op (const m : foobar) : foobar is
  block {
    function increment (const _ : int; const j : int) : int is j+1
  } with Map.map (increment, m)

function fold_op (const m : foobar) : int is
  block {
    function aggregate (const i : int; const j : int * int) : int is
      i + j.0 + j.1
  } with Map.fold (aggregate, m, 10)

function deep_op (var m : foobar) : foobar is
  block {
    var coco : int * foobar := (0, m);
    remove 42 from map coco.1;
    coco.1[32] := 16
  } with coco.1
