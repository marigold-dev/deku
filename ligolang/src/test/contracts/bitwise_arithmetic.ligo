// Test PascaLIGO bitwise operators

function or_op  (const n : nat) : nat is Bitwise.or (n, 4n)
function and_op (const n : nat) : nat is Bitwise.and (n, 7n)
function xor_op (const n : nat) : nat is Bitwise.xor (n, 7n)
function lsl_op (const n : nat) : nat is Bitwise.shift_left (n, 7n)
function lsr_op (const n : nat) : nat is Bitwise.shift_right (n, 7n)
