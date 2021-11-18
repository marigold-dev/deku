// Test set type and basic operations in PascaLIGO

const s_e : set (string) = set_empty

const s_fb : set (string) = set ["foo"; "bar"]

function add_op (const s : set (string)) : set (string) is
  set_add ("foobar", s)

function remove_op (const s : set (string)) : set (string) is
  set_remove ("foobar", s)

// Test the PascaLIGO syntactic sugar for set removal vs. the function call
function remove_syntax (var s : set (string)) : set (string) is
  block {remove "foobar" from set s} with s

function remove_deep (var s : set (string) * nat) : set (string) * nat is
  block {remove "foobar" from set s.0} with s

function patch_op (var s : set (string)) : set (string) is
  block {patch s with set ["foobar"]} with s

function patch_op_deep (var s : set (string) * nat) : set (string) * nat is
  block {patch s.0 with set ["foobar"]} with s

function mem_op (const s : set (string)) : bool is
  set_mem ("foobar", s)
