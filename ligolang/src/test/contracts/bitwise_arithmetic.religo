/* Test ReasonLigo bitwise operators */

let or_op  = (n : nat) : nat => Bitwise.or (n, 4n);
let and_op = (n : nat) : nat => Bitwise.and (n, 7n);
let xor_op = (n : nat) : nat => Bitwise.xor (n, 7n);
let lsl_op = (n : nat) : nat => Bitwise.shift_left (n, 7n);
let lsr_op = (n : nat) : nat => Bitwise.shift_right (n, 7n);

let or_op_infix  = (n : nat) : nat => n lor 4n;
let and_op_infix = (n : nat) : nat => n land 7n;
let xor_op_infix = (n : nat) : nat => n lxor 7n;
let lsl_op_infix = (n : nat) : nat => n lsl 7n;
let lsr_op_infix = (n : nat) : nat => n lsr 7n;