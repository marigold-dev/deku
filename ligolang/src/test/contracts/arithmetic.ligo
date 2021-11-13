function mod_op   (const n : int) : nat is n mod 42
function plus_op  (const n : int) : int is n + 42
function minus_op (const n : int) : int is n - 42
function times_op (const n : int) : int is n * 42
function div_op   (const n : int) : int is n / 2
function int_op   (const n : nat) : int is int (n)
function neg_op   (const n : int) : int is -n
function ediv_op  (const n : int) : option (int * nat) is ediv (n,2)
