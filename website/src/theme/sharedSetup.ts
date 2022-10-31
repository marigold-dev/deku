import { InMemorySigner } from "@taquito/signer";
import { DekuCClient } from "@marigold-dev/deku-c-toolkit";
import { fromMemorySigner } from "@marigold-dev/deku-toolkit";

const memory = new InMemorySigner(
  "edsk3ym86W81aL2gfZ25WuWQrisJM5Vu8cEayCR6BGsRNgfRWos8mR"
);

const signer = fromMemorySigner(memory);

// More convenient for dev/testing
// TODO: remove this when ligoRpc and dekuRpc are reployed

const isLocalhost = window.location.hostname === "localhost";
const dekuRpc = isLocalhost
  ? "http://0.0.0.0:8080"
  : "https://deku-canonical-vm0.deku-v1.marigold.dev";
const ligoRpc = isLocalhost
  ? "http://0.0.0.0:9090"
  : "https://ghostnet.tezos.marigold.dev";

console.log(dekuRpc);
console.log(ligoRpc);

const dekuC = new DekuCClient({
  dekuRpc,
  ligoRpc,
  signer,
});

const incrementLigoCode = `
type storage = int;

type parameter =
  | ["Increment", int]
  | ["Decrement", int]
  | ["Reset"];

type return_ = [list<operation>,storage];

const main = (action: parameter, store: storage): return_ => {
  let storage = match(action, {
    Increment: n => store + n,
    Decrement: n => store - n,
    Reset: () => 0
  });
  return [list([]), storage]
};
`;

const incrementWASMCode = `
    (module
      (import "env" "dup_host" (func $dup_host (param i64 ) (result)))
    (import "env" "pair" (func $pair (param i64 i64) (result i64)))
    (import "env" "unpair" (func $unpair (param i64)))
    (import "env" "z_add" (func $z_add (param i64 i64) (result i64)))
    (import "env" "z_sub" (func $z_sub (param i64 i64) (result i64)))
    (import "env" "z_mul" (func $z_mul (param i64 i64) (result i64)))
    (import "env" "neg" (func $neg (param i64) (result i64)))
    (import "env" "lsl" (func $lsl (param i64 i64) (result i64)))
    (import "env" "lsr" (func $lsr (param i64 i64) (result i64)))
    (import "env" "compare" (func $compare (param i64 i64) (result i64)))
    (import "env" "car" (func $car (param i64) (result i64)))
    (import "env" "cdr" (func $cdr (param i64) (result i64)))
    (import "env" "some" (func $some (param i64) (result i64)))
    (import "env" "nil" (func $nil (result i64)))
    (import "env" "none" (func $none (result i64)))
    (import "env" "unit" (func $unit (result i64)))
    (import "env" "zero" (func $zero (result i64)))
    (import "env" "empty_map" (func $empty_map (result i64)))
    (import "env" "empty_set" (func $empty_set (result i64)))
    (import "env" "empty_big_map" (func $empty_big_map (result i64)))
    (import "env" "sender" (func $sender (result i64)))
    (import "env" "source" (func $source (result i64)))
    (import "env" "map_get" (func $map_get (param i64 i64) (result i64)))
    (import "env" "mem" (func $mem (param i64 i64) (result i64)))
    (import "env" "update" (func $update (param i64 i64 i64) (result i64)))
    (import "env" "iter" (func $iter (param i64 i32) (result )))
    (import "env" "map" (func $map (param i64 i32) (result i64)))
    (import "env" "if_left" (func $if_left (param i64) (result i32)))
    (import "env" "if_none" (func $if_none (param i64) (result i32)))
    (import "env" "if_cons" (func $if_cons (param i64) (result i32)))
    (import "env" "isnat" (func $isnat (param i64) (result i64)))
    (import "env" "not" (func $not (param i64) (result i64)))
    (import "env" "or" (func $or (param i64 i64) (result i64)))
    (import "env" "and" (func $and (param i64 i64) (result i64)))
    (import "env" "xor" (func $xor (param i64 i64) (result i64)))
    (import "env" "deref_bool" (func $deref_bool (param i64) (result i32)))
    (import "env" "neq" (func $neq (param i64) (result i64)))
    (import "env" "failwith" (func $failwith (param i64)))
    (import "env" "get_n" (func $get_n (param i32 i64) (result i64)))
    (import "env" "exec" (func $exec (param i64 i64) (result i64)))
    (import "env" "apply" (func $apply (param i64 i64) (result i64)))
    (import "env" "const" (func $const (param i32) (result i64)))
    (import "env" "abs" (func $abs (param i64) (result i64)))
    (import "env" "eq" (func $eq (param i64) (result i64)))
    (import "env" "gt" (func $gt (param i64) (result i64)))
    (import "env" "lt" (func $lt (param i64) (result i64)))
    (import "env" "closure" (func $closure (param i32) (result i64)))
    (import "env" "left" (func $left (param i64) (result i64)))
    (import "env" "right" (func $right (param i64) (result i64)))
    (import "env" "cons" (func $cons (param i64 i64) (result i64)))
    (import "env" "transfer_tokens" (func $transfer_tokens (param i64 i64 i64) (result i64)))
    (import "env" "address" (func $address (param i64) (result i64)))
    (import "env" "contract" (func $contract (param i64) (result i64)))
    (import "env" "self" (func $self (result i64)))
    (import "env" "self_address" (func $self_address (result i64)))
    (import "env" "get_and_update" (func $get_and_update (param i64 i64 i64)))
    (import "env" "read_ticket" (func $read_ticket (param i64)))
    (import "env" "ticket" (func $ticket (param i64 i64) (result i64)))
    (import "env" "join_tickets" (func $join_tickets (param i64) (result i64)))
    (import "env" "split_ticket" (func $split_ticket (param i64 i64) (result i64)))
    (import "env" "amount" (func $amount (result i64)))
    (import "env" "balance" (func $balance (result i64)))
    (import "env" "ediv" (func $ediv (param i64 i64) (result i64)))
    (import "env" "ge" (func $ge (param i64) (result i64)))
    (import "env" "le" (func $le (param i64) (result i64)))
    (import "env" "size" (func $size (param i64) (result i64)))
    (import "env" "int" (func $int (param i64) (result i64)))
    (import "env" "implicit_account" (func $implicit_account (param i64) (result i64)))
    (import "env" "blake2b" (func $blake2b (param i64) (result i64)))
    (import "env" "pack" (func $pack (param i64) (result i64)))
    (import "env" "unpack" (func $unpack (param i64) (result i64)))
    (import "env" "keccak" (func $keccak (param i64) (result i64)))
    (import "env" "sha256" (func $sha256 (param i64) (result i64)))
    (import "env" "sha3" (func $sha3 (param i64) (result i64)))
    (import "env" "sha512" (func $sha512 (param i64) (result i64)))
    
      (global $mode i32 (i32.const 0))
    
      (memory 4)
      (global $sp (mut i32) (i32.const 4000)) ;; stack pointer
      (global $sh_sp (mut i32) (i32.const 1000)) ;;shadow_stack stack pointer
    
      (global $__stack_base i32 (i32.const 32768))
    
      (type $callback_t (func (param i64) (result i64)))
      (func $call_callback (param $arg1 i64) (param $idx i32) (result i64)
        (call_indirect (type $callback_t) (local.get $arg1) (local.get $idx)))
    
      (type $callback_t_unit (func (param i64) (result)))
      (func $call_callback_unit (param $arg1 i64) (param $idx i32) (result )
        (call_indirect (type $callback_t_unit)
          (local.get $arg1)
          (local.get $idx)))
    
      (func $dip (param $n i32) (result)
        (local $stop i32)
        (local $sp' i32)
        (local $sh_sp' i32)
        (local.set $stop (i32.const 0))
        (local.set $sp'  (global.get $sp))
        (local.tee $sh_sp' (i32.sub (global.get $sh_sp) (local.get $n)))
        global.set $sh_sp
        (loop $l
          (i32.mul (i32.const 8) (i32.add (global.get $__stack_base) (i32.add (local.get $sh_sp') (local.get $stop))))
          (i64.load (i32.mul (i32.const 8) (i32.add (local.get $sp') (local.get $stop))))
          i64.store
          (local.tee $stop (i32.add (local.get $stop) (i32.const 1)))
          (local.get $n)
          i32.ne
          br_if $l)
    
        (global.set $sp
        (i32.add
          (local.get $sp') (local.get $n))))
    
      (func $undip (param $n i32) (result)
        (local $stop i32)
        (local $sp' i32)
        (local $sh_sp' i32)
        (local.tee $sp'  (i32.sub (global.get $sp) (local.get $n)))
        global.set $sp
        (local.set $sh_sp' (global.get $sh_sp))
        (local.set $stop (i32.const 0))
        (loop $l
          (i32.mul (i32.const 8) (i32.add (local.get $sp') (local.get $stop)))
          (i64.load
            (i32.add
              (global.get $__stack_base)
              (i32.mul (i32.const 8) (i32.add (local.get $sh_sp') (local.get $stop)))))
          (i64.store)
          (local.tee $stop (i32.add (local.get $stop) (i32.const 1)))
          (local.get $n)
          i32.ne
          br_if $l)
        (global.set $sh_sp (i32.add (local.get $sh_sp') (local.get $n))))
    
      (func $dup (param $n i32) (result)
        (i64.load (i32.mul (i32.const 8) (i32.add (global.get $sp) (local.get $n))))
        (call $dup_host))
    
      (func $swap (param) (result)
        (local $v1 i64)
        (local $v2 i64)
        (local.set $v1 (call $pop))
        (local.set $v2 (call $pop))
        (call $push (local.get $v1))
        (call $push (local.get $v2)))
    
      (func $dug (param $n i32) (result)
        (local $idx i32)
        (local $loop_idx i32)
        (local $sp' i32)
        (local $top i64)
        (local.set $sp' (i32.add (global.get $sp) (local.get $n)))
        (local.tee $idx (global.get $sp))
        (local.tee $loop_idx)
        (i32.mul (i32.const 8))
        i64.load
        local.set $top
        (loop $loop
          (i32.mul (i32.const 8) (local.get $idx))
          (i32.add (local.get $loop_idx) (i32.const 1))
          local.tee $loop_idx
          (i32.mul (i32.const 8))
          i64.load
          i64.store
          (local.set $idx (i32.add (local.get $idx) (i32.const 1)))
          (local.get $idx)
          (local.get $sp')
          i32.lt_u
          br_if $loop)
    
        (i64.store (i32.mul (i32.const 8) (local.get $sp')) (local.get $top)))
    
      (func $dig (param $n i32) (result)
        (local $idx i32) (local $t i32) (local $digged i64)
    
        (local.set $digged
          (i64.load
            (i32.mul (i32.const 8)
              (local.tee $idx (i32.add (global.get $sp) (local.get $n))))))
    
        (loop $loop
          (local.set $t (i32.mul (i32.const 8) (local.get $idx)))
    
          (i64.store (local.get $t)
            (i64.load
              (i32.mul
                (i32.const 8)
                (local.tee $idx (i32.sub (local.get $idx) (i32.const 1))))))
    
          (br_if $loop
            (i32.lt_u (global.get $sp) (local.get $idx))))
    
        (i64.store (i32.mul (i32.const 8) (local.get $idx)) (local.get $digged)))
    
      (func $pop (result i64)
        (local $spp i32)
        (i32.mul (i32.const 8) (local.tee $spp (global.get $sp)))
        i64.load
        (global.set $sp (i32.add (local.get $spp) (i32.const 1))))  ;;set stackptr
    
      (func $push (param $value i64) (result)
        (local $spp i32)
        (i32.mul (i32.const 8) (local.tee $spp (i32.sub (global.get $sp) (i32.const 1)) ))
        (i64.store (local.get $value))
        (global.set $sp (local.get $spp)))  ;;set stackptr
    
      (func $drop (param $n i32) (result)
        (global.set $sp (i32.add (global.get $sp) (local.get $n))))  ;;set stackptr
    
      (table $closures funcref (elem ))
    
    
      (func $main (param $v1 i64) (result i64)
        (local $1 i64)
        (call $push (local.get $v1))
        (call $unpair (call $pop)) ;; implicit return
    (call $if_left (call $pop)) (if (then (call $if_left (call $pop)) (if (then (call $swap)
    (call $push (call $z_sub (call $pop) (call $pop)))) (else (call $push (call $z_add (call $pop) (call $pop)))))) (else (call $drop (i32.const 2))
    (call $push (call $zero)) (; 0 ;)))
    (call $push (call $nil))
    (call $push (call $pair (call $pop) (call $pop)))
        (call $pop))
    
      (export "push" (func $push))
      (export "pop" (func $push))
      (export "main" (func $main))
      (export "closures" (table $closures))
      (export "call_callback" (func $call_callback))
      (export "call_callback_unit" (func $call_callback_unit))
      )
    `;

export default {
  incrementLigoCode,
  incrementWASMCode,
  dekuC,
  typeUtilities,
};
