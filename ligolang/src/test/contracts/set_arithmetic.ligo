// Test set type and basic operations in PascaLIGO

const s_e : set (string) = set_empty

const s_fb : set (string) = set ["foo"; "bar"]

function literal_op (const _: unit) : set (string) is
  set ["foo"; "bar"; "foobar"]

function size_op (const s: set (string)) : nat is Set.cardinal (s)

function add_op (const s : set (string)) : set (string) is
  set_add ("foobar", s)

function remove_op (const s : set (string)) : set (string) is
  set_remove ("foobar", s)

// Test the PascaLIGO syntactic sugar for set removal vs. the function call
function remove_deep (var s : set (string) * nat) : set (string) * nat is
  block {remove "foobar" from set s.0} with s

function patch_op (var s : set (string)) : set (string) is
  block {patch s with set ["foobar"]} with s

function patch_op_deep (var s : set (string) * nat) : set (string) * nat is
  block {patch s.0 with set ["foobar"]} with s

function mem_op (const s : set (string)) : bool is
  set_mem ("foobar", s)

function iter_op (const s : set (int)) : int is
  block {
    var r : int := 0;
    set_iter ((function (const _i : int) : unit is unit), s)
  } with r // ALWAYS RETURNS 0

function iter_op_with_effect (const s : set (int)) : int is
  block {
    var r : int := 0;
    function aggregate (const _i : int) : unit is
      block {
        skip (* r := r + 1 Todo : solve capture problem *)
      } with unit;
    set_iter (aggregate, s)
  } with r // ALWAYS RETURNS 0

function fold_op (const s : set (int)) : list(int) is
  block {
    function aggregate (const i : list(int); const j : int) : list(int) is j # i
  } with set_fold (aggregate, s, list [])

function fold_right (const s : set (int)) : list(int) is
  block {
    function aggregate (const i : int; const j : list(int)) : list(int) is i # j
  } with Set.fold_desc (aggregate, s, list [])