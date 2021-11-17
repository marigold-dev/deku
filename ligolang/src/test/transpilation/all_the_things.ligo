type tokens     is big_map (address, nat)
type allowances is big_map (address * address, nat) (* (sender,account) -> value *)

type storage is record [
  tokens      : tokens;
  allowances  : allowances;
  total_amount : nat;
]

type transfer is record [
	address_from : address;
	address_to   : address;
	value        : nat;
]

type approve is record [
	spender : address;
	value   : nat;
]

type getAllowance is record [
	owner    : address;
	spender  : address;
	callback : contract (nat);
]

type getBalance is record [
	owner    : address;
	callback : contract (nat);
]

type getTotalSupply is record [
	callback : contract (nat);
]

type action is
  	Transfer       of transfer
|	Approve        of approve
|	GetAllowance   of getAllowance
|	GetBalance     of getBalance
|	GetTotalSupply of getTotalSupply

function transfer (const p : transfer; const s: storage) : list (operation) * storage is block {
   var new_allowances : allowances := Big_map.empty;
	if Tezos.sender = p.address_from
	then { new_allowances := s.allowances; }
	else {
		var authorized_value : nat := 
		case (Big_map.find_opt ((Tezos.sender,p.address_from), s.allowances)) of
				Some (value) -> value
			|	None       -> 0n
		end;
		if (authorized_value < p.value)
		then { failwith("Not Enough Allowance")}
		else { new_allowances := Big_map.update ((Tezos.sender,p.address_from), (Some (abs(authorized_value - p.value))), s.allowances) }    
	};
	var sender_balance : nat := case (Big_map.find_opt (p.address_from, s.tokens)) of
		Some (value) -> value
	|	None        -> 0n
	end;
	var new_tokens : tokens := Big_map.empty;
	if (sender_balance < p.value)
	then { failwith ("Not Enough Balance")}
	else {
		new_tokens := Big_map.update (p.address_from, (Some (abs(sender_balance - p.value))), s.tokens);
		var receiver_balance : nat := case (Big_map.find_opt (p.address_to, s.tokens)) of
			Some (value) -> value
		|	None        -> 0n
		end;
		new_tokens := Big_map.update (p.address_to, (Some (receiver_balance + p.value)), new_tokens);
	}
} with ((nil: list (operation)), s with record [tokens = new_tokens; allowances = new_allowances])

function approve (const p : approve; const s : storage) : list (operation) * storage is block {
	var previous_value : nat := case Big_map.find_opt ((p.spender, Tezos.sender), s.allowances) of
		Some (value) -> value
	|	None -> 0n
	end;
	var new_allowances : allowances := Big_map.empty;
	if previous_value > 0n and p.value > 0n
	then { failwith ("Unsafe Allowance Change")}
	else {
		new_allowances := Big_map.update ((p.spender, Tezos.sender), (Some (p.value)), s.allowances);
	}
} with ((nil: list (operation)), s with record [allowances = new_allowances])

function getAllowance (const p : getAllowance; const s : storage) : list (operation) * storage is block {
	var value : nat := case Big_map.find_opt ((p.owner, p.spender), s.allowances) of
		Some (value) -> value
	|	None -> 0n
	end;
	var op : operation := Tezos.transaction (value, 0mutez, p.callback);
} with (list [op],s)

function getBalance (const p : getBalance; const s : storage) : list (operation) * storage is block {
	var value : nat := case Big_map.find_opt (p.owner, s.tokens) of
		Some (value) -> value
	|	None -> 0n
	end;
	var op : operation := Tezos.transaction (value, 0mutez, p.callback);
} with (list [op],s)

function getTotalSupply (const p : getTotalSupply; const s : storage) : list (operation) * storage is block {
  var total : nat := s.total_amount;
  var op : operation := Tezos.transaction (total, 0mutez, p.callback);
} with (list [op],s)


function main (const a : action; const s : storage) : list (operation) * storage is
 	case a of
   	Transfer       (p) -> transfer (p,s)
	|	Approve        (p) -> approve (p,s)
	|	GetAllowance   (p) -> getAllowance (p,s)
	|  GetBalance     (p) -> getBalance (p,s)
	|	GetTotalSupply (p) -> getTotalSupply (p,s)
	end;
function main (const p : key_hash) : address is block {
  const c : contract (unit) = Tezos.implicit_account (p);
} with Tezos.address (c)
function check (const p : unit) : int is
  block {
    var result : int := 0;
    if amount = 100tez then result := 42 else result := 0
  } with result
(* Test that a string is cast to an address given a type annotation *)

const lst : list (int) = list []

const my_address : address =
  ("tz1KqTpEZ7Yob7QbPE4Hy4Wo8fHG8LhKxZSx" : address)
// Test different ways of calling functions in PascaLIGO

type foo is record [bar : int -> int]

function f (const i : int) : int is i

function g (const i : unit) : int -> int is f

const r : foo = record [bar = f]

const x : int = f (42)
const y : int = r.bar (42)
const z : int = (g (unit))(42)
function mod_op   (const n : int) : nat is n mod 42
function plus_op  (const n : int) : int is n + 42
function minus_op (const n : int) : int is n - 42
function times_op (const n : int) : int is n * 42
function div_op   (const n : int) : int is n / 2
function int_op   (const n : nat) : int is int (n)
function neg_op   (const n : int) : int is -n
function ediv_op  (const n : int) : option (int * nat) is ediv (n,2)
function main (var i : int) : int is block {i := i + 1} with i
[@annot] const x : int = 1;

[@inline] function foo (const a : int) : int is
  block {
    [@inline] const test : int = 2 + a;
  } with test;

[@inline][@other] const y : int = 1;

function bar (const b : int) : int is
  block {
    [@inline][@foo][@bar]
    function test (const z : int) : int is
      block {
        const r : int = 2 + b + z
      } with r;
  } with test (b)
  
type parameter is unit

type binding is nat * nat
type storage is map (binding)

type return is list (operation) * storage

function main (const param : parameter; const store : storage) : return is
  ((nil : list (operation)), store)
(*
This test makes sure that the balance is accessible in PascaLIGO.

It is meant to detect the regression detailled in the following issue: https://gitlab.com/ligolang/ligo/issues/68
*)

type parameter is unit
type storage is tez
type return is list (operation) * storage

function main (const param : parameter; const store: storage) : return is
  ((nil : list (operation)), Tezos.balance)
type parameter is unit
type storage is big_map (int, int) * unit
type return is list (operation) * storage

function main (const p : parameter; var s : storage) : return is
  block {
    var toto : option (int) := Some (0);
    toto := s.0[23];
    s.0[2] := 444
  }
  with ((nil : list (operation)), s)

type foo is big_map (int, int)

function set_ (var n : int; var m : foo) : foo is block {
  m[23] := n
} with m

function add (var n : int ; var m : foo) : foo is set_ (n,m)

function rm (var m : foo) : foo is block {
  remove 42 from map m
} with m

function get (const m : foo) : option (int) is m[42]

const empty_big_map : big_map (int,int) = big_map []

const big_map1 : big_map (int,int) = big_map [23 -> 0; 42 -> 0]

function mutimaps (const m : foo; var n : foo) : foo is block {
  var bar : foo := m;
  bar[42] := 0;
  n[42] := get_force (42, bar)
} with n
// Test PascaLIGO bitwise operators

function or_op  (const n : nat) : nat is Bitwise.or (n, 4n)
function and_op (const n : nat) : nat is Bitwise.and (n, 7n)
function xor_op (const n : nat) : nat is Bitwise.xor (n, 7n)
function lsl_op (const n : nat) : nat is Bitwise.shift_left (n, 7n)
function lsr_op (const n : nat) : nat is Bitwise.shift_right (n, 7n)
function blockless (const n : int) : int is n + 10
function or_true   (const b : bool) : bool is b or True
function or_false  (const b : bool) : bool is b or False
function and_true  (const b : bool) : bool is b and True
function and_false (const b : bool) : bool is b and False
function not_bool  (const b : bool) : bool is not b
function concat_op (const s : bytes) : bytes is Bytes.concat (s, 0x7070)
function slice_op  (const s : bytes) : bytes is Bytes.sub (1n, 2n, s)
function hasherman (const s : bytes) : bytes is Crypto.sha256 (s)
function id_string (const p : string) : option (string) is block {
  const packed : bytes = Bytes.pack (p)
} with (Bytes.unpack (packed) : option (string))

function id_int (const p : int) : option (int) is block {
  const packed : bytes = Bytes.pack (p)
} with (Bytes.unpack (packed) : option (int))

function id_address (const p : address) : option (address) is block {
  const packed : bytes = Bytes.pack (p)
} with (Bytes.unpack (packed) : option (address))
function chain_id (const tt : chain_id) : chain_id is Tezos.chain_id
function check_signature (const pk     : key;
                          const signed : signature;
                          const msg    : bytes) : bool
is Crypto.check (pk, signed, msg)
function foo (const i : int) : int is
  block {
    function add (const j : int) : int is i+j
  } with add (i)
function foobar (const i : int) : int is
  block {
    const j : int = 3;
    function add (const k : int) : int is i+j+k
  } with add (42)
// This might seem like it is covered by induction with closure-2.ligo,
// but it exists to prevent a regression on the bug patched by:
// https://gitlab.com/ligolang/ligo/commit/faf3bbc06106de98189f1c1673bd57e78351dc7e

function foobar (const i : int) : int is
  block {
    const j : int = 3;
    const k : int = 4;
    function add (const l : int) : int is i+j+k+l
  } with add (42)
function toto (const i : int) : int is
  block {
    function tata (const j : int) : int is i+j;
    function titi (const j : int) : int is i+j
  } with tata (i) + titi (i)
// Copyright Coase, Inc 2019

type card_pattern_id is nat

type card_pattern is record [
  coefficient : tez;
  quantity    : nat
]

type card_patterns is map (card_pattern_id, card_pattern)

type card_id is nat

type card is record [
  card_owner   : address;
  card_pattern : card_pattern_id
]

type cards is map (card_id, card)

type storage is record [
  cards         : cards;
  card_patterns : card_patterns;
  next_id       : nat
]

type return is list (operation) * storage

type action_buy_single is record [
  card_to_buy : card_pattern_id
]

type action_sell_single is record [
  card_to_sell : card_id
]

type action_transfer_single is record [
  card_to_transfer : card_id;
  destination      : address
]

type parameter is
  Buy_single      of action_buy_single
| Sell_single     of action_sell_single
| Transfer_single of action_transfer_single

function transfer_single (const action : action_transfer_single;
                          var s : storage) : return is
  block {
    var cards : cards := s.cards;
    var card : card :=
      case cards[action.card_to_transfer] of
        Some (card) -> card
      | None -> (failwith ("transfer_single: No card.") : card)
      end;
    if card.card_owner =/= sender then
      failwith ("This card doesn't belong to you")
    else skip;
    card.card_owner := action.destination;
    cards[action.card_to_transfer] := card;
    s.cards := cards
  } with ((nil : list (operation)), s)

function sell_single (const action : action_sell_single;
                      var s : storage) : return is
  block {
    const card : card =
      case s.cards[action.card_to_sell] of
        Some (card) -> card
      | None -> (failwith ("sell_single: No card.") : card)
      end;
    if card.card_owner =/= sender
    then failwith ("This card doesn't belong to you")
    else skip;
    var card_pattern : card_pattern :=
      case s.card_patterns[card.card_pattern] of
        Some (pattern) -> pattern
      | None -> (failwith ("sell_single: No card pattern.") : card_pattern)
      end;
    card_pattern.quantity := abs (card_pattern.quantity - 1n);
    var card_patterns : card_patterns := s.card_patterns;
    card_patterns[card.card_pattern] := card_pattern;
    s.card_patterns := card_patterns;
    var cards : cards := s.cards;
    remove action.card_to_sell from map cards;
    s.cards := cards;
    const price : tez = card_pattern.coefficient * card_pattern.quantity;
    const receiver : contract (unit) =
      case (Tezos.get_contract_opt (Tezos.sender) : option (contract (unit))) of
        Some (contract) -> contract
      | None -> (failwith ("sell_single: No contract.") : contract (unit))
      end;
    const op : operation = Tezos.transaction (unit, price, receiver);
    const operations : list (operation) = list [op]
  } with (operations, s)

function buy_single (const action : action_buy_single;
                     var s : storage) : return is
  block {
    // Check funds
    var card_pattern : card_pattern :=
      case s.card_patterns[action.card_to_buy] of
        Some (pattern) -> pattern
      | None -> (failwith ("buy_single: No card pattern.") : card_pattern)
      end;
    const price : tez =
      card_pattern.coefficient * (card_pattern.quantity + 1n);
    if price > amount then failwith ("Not enough money") else skip;
    // Increase quantity
    card_pattern.quantity := card_pattern.quantity + 1n;
    var card_patterns : card_patterns := s.card_patterns;
    card_patterns[action.card_to_buy] := card_pattern;
    s.card_patterns := card_patterns;
    // Add card
    var cards : cards := s.cards;
    cards[s.next_id] := record [
      card_owner   = sender;
      card_pattern = action.card_to_buy
    ];
    s.cards := cards;
    s.next_id := s.next_id + 1n
  } with ((nil : list (operation)), s)

function main (const action : parameter; const s : storage) : return is
  case action of
    Buy_single (bs)      -> buy_single (bs, s)
  | Sell_single (as)     -> sell_single (as, s)
  | Transfer_single (at) -> transfer_single (at, s)
  end
function main (const i : int) : int is if 1 = 1 then 42 else 0
function main (const i : int) : int is
  block {
    var result : int := 23;
    if i = 2 then result := 42 else result := 0
  } with result

function foo (const b : bool) : int is
  block {
    const x : int = 41
  } with 1 + (if b then x else main (x))
type t is int

function main (const p : int; const s : t) : list (operation) * int is
  block {
    skip
  } // skip is a do nothing instruction, needed for empty blocks
  with ((nil : list (operation)), p+s)
function hasherman512    (const s : bytes) : bytes is Crypto.sha512 (s)
function hasherman_blake (const s : bytes) : bytes is Crypto.blake2b (s)
// Test PasaLIGO variable declarations inside of a block

function main (const i : int) : int is block {
  const j : int = 42
} with j
const foo : int = 42

function main (const i : int) : int is i + foo
// Test deep access

type pii is int * int

type ppi is record [x : pii; y : pii]

type ppp is ppi * ppi

function main (const toto : unit) : int is
  block {
    var a : ppp :=
     (record [x = (0,1); y = (10,11)],
      record [x = (100,101); y = (110,111)]);
    a.0.x.0 := 2;
  } with a.0.x.0


function asymetric_tuple_access (const foo : unit) : int is
  block {
    var tuple : int * (int * (int * int)) := (0,(1,(2,3)))
  } with tuple.0 + tuple.1.0 + tuple.1.1.0 + tuple.1.1.1

type nested_record_t is
  record [nesty : record [mymap : map (int, string)]]

function nested_record (var nee : nested_record_t) : string is
  block {
    nee.nesty.mymap[1] := "one"
  } with case nee.nesty.mymap[1] of
           Some (s) -> s
         | None -> (failwith ("Should not happen.") : string)
         end
type parameter is
  Increment of int
| Decrement of int

type storage is int

type return is list (operation) * storage

function increment (const i : int; const n : int) : int is i+n
function decrement (const i : int; const n : int) : int is i-n

const nop : list (operation) = nil

function main (const action : parameter; const store : storage) : return is
  case action of
    Increment (n) -> (nop, increment (store, n))
  | Decrement (n) -> (nop, decrement (store, n))
  end
type parameter is unit
type storage is int
type return is list (operation) * storage

function main(const p : parameter; const s : storage) : return is
  ((nil : list(operation)), s+1)

function main (const p : parameter; const s : storage) : return is
  block {
    const ret : return = main (p, s)
  } with (ret.0, ret.1 + 1)
type storage is michelson_or (int,"foo",string,"bar")
type foobar is michelson_or (int,"baz",int,"fooo")

type return is list (operation) * storage 

function main (const action : unit; const store : storage) : return is
block { 
  const foo : storage = (M_right ("one") : storage);
  const bar : foobar = (M_right (1) : foobar)
} with
 ((nil : list (operation)), (foo : storage))type t is
  Bar of int
| Baz

function main (const x : t) : int is
  case x of
    Bar (n) -> n
  | Baz     -> -1
  end
type storage is unit

type return is list (operation) * storage

function cb (const a : address; const s : storage) : return is
  block {
    const c : contract (unit) = get_entrypoint ("%cb", a)
  } with (list [Tezos.transaction (unit, 0tez, c)], s)


function cbo (const a : address; const s : storage) : return is
  block {
    const c : contract (unit) =
      case (get_entrypoint_opt ("%cbo", a) : option (contract (unit))) of
        Some (c) -> c
      | None -> (failwith ("cbo: Entrypoint not found.") : contract (unit))
      end
  } with (list [Tezos.transaction (unit, 0tez, c)], s)
function main (const a : bool; const b : bool) : int is
  block {
    var result : int := 27;
    if a = b then result := 999 else result := 1
  } with result
type t is record [foo : nat; bar : string]

const a : t = record [foo = 0n; bar = "bar"]

const b : int = 2
type parameter is
  Zero of nat
| Pos  of nat

type storage is unit

type return is list (operation) * storage

function main (const p : parameter; const s : storage) : return is
  block {
    case p of
      Zero (n) -> if n > 0n then failwith ("fail") else skip
    | Pos (n)  -> if n > 0n then skip else failwith ("fail")
    end
  }
  with ((nil : list (operation)), s)

function foobar (var i : int) : int is
  block {
    var p : parameter := Zero (42n);
    if i > 0 then {
      i := i + 1;
      if i > 10 then {
        i := 20;
        failwith ("who knows");
        i := 30 // Should be no-op
      }
      else skip
    }
    else
      case p of
        Zero (n) -> failwith(42n)
      | Pos (n)  -> skip
      end
  } with
      case p of
        Zero (n) -> i
      | Pos (n)  -> (failwith ("waaaa") : int)
      end

function failer (const p : int) : int is block {
  if p = 1 then failwith (42) else skip
} with p
function main (const a : int) : int is
  block { for i := 0 to 100 block { skip } } with i
const x : int = (function (const i : int) : int is i + 1)(41)
// Test a PascaLIGO function with more complex logic than function.ligo

function main (const i : int) : int is
  block {
    var j : int := 0;
    var k : int := 1;
    j := k + i;
    k := i + j
  } with k + j
// Test a PascaLIGO function which uses other functions as subroutines

function inc (const i : int) : int is i+1

function double_inc (const i : int) : int is inc (i+1)

function foo (const i : int) : int is inc (i) + double_inc (i)
// Test a trivial PascaLIGO function

function main (const i : int) : int is i
type storage is unit
type return is list (operation) * storage

function cb (const s : storage) : return is block {
  const c : contract (unit) = get_contract (Tezos.sender)
} with (list [Tezos.transaction (unit, 0tez, c)], s)


function cbo (const s : unit) : return is
  block {
    const c : contract (unit) =
      case (Tezos.get_contract_opt (Tezos.sender) : option (contract (unit))) of
        Some (contract) -> contract
      | None -> (failwith ("contract not found") : contract (unit))
      end
  } with (list [Tezos.transaction (unit, 0tez, c)], s)
type heap_elt is int * string

function heap_elt_lt (const x : heap_elt;
                      const y : heap_elt) : bool is x.0 < y.0

// Implementation of the heap data structure in PascaLIGO
// See: https://en.wikipedia.org/wiki/Heap_%28data_structure%29

type heap is map (nat, heap_elt)

function is_empty (const h : heap) : bool is size (h) = 0n

function get_top (const h : heap) : heap_elt is get_force (1n, h)

function pop_switch (var h : heap) : heap is
  block {
   const result : heap_elt = get_top (h);
   const s : nat = Map.size (h);
   const last : heap_elt =
     case h[s] of
       Some (e) -> e
     | None -> (failwith ("No element.") : heap_elt)
     end;
   remove 1n from map h;
   h[1n] := last
  } with h

function pop_ (var h : heap) : nat is
  block {
    const result : heap_elt = get_top (h);
    const s : nat = Map.size (h);
    var current : heap_elt :=
      case h[s] of
        Some (e) -> e
      | None -> (failwith ("No element.") : heap_elt)
      end;
    const i : nat = 1n;
    const left : nat = 2n * i;
    const right : nat = left + 1n;
    remove 1n from map h;
    h[1n] := current;
    var largest : nat := i;
    const tmp : heap_elt = get_force (s, h);
    if left <= s and heap_elt_lt (tmp, get_force (left,h))
    then largest := left
    else
      if right <= s and heap_elt_lt (tmp, get_force (right,h))
      then largest := right
      else skip
  } with largest

function insert (var h : heap ; const e : heap_elt) : heap is
  block {
    var i : nat := size (h) + 1n;
    h[i] := e;
    var largest : nat := i;
    var parent : nat := 0n;
    while largest =/= i block {
      parent := i/2n;
      largest := i;
      if parent >= 1n then {
        if heap_elt_lt (get_force (parent,h), get_force(i,h)) then {
          largest := parent;
          const tmp : heap_elt = get_force (i,h);
          h[i] := get_force(parent, h);
          h[parent] := tmp
        } else skip
      } else skip
    }
  } with h

function pop (var h : heap) : heap * heap_elt * nat is
  block {
    const result : heap_elt = get_top (h);
    var s : nat := size (h);
    const last : heap_elt = get_force (s,h);
    remove s from map h;
    h[1n] := last;
    s := size (h);
    var i : nat := 0n;
    var largest : nat := 1n;
    var left : nat := 0n;
    var right : nat := 0n;
    var c : nat := 0n;
    while largest =/= i block {
      c := c + 1n;
      i := largest;
      left := 2n * i;
      right := left + 1n;
      if left <= s then {
        if heap_elt_lt (get_force (left,h), get_force(i,h)) then {
          largest := left;
          const tmp : heap_elt = get_force(i,h);
          h[i] := get_force (left, h);
          h[left] := tmp
        } else skip
      }
      else
        if right <= s then {
          if heap_elt_lt (get_force (right, h), get_force (i,h)) then {
            largest := right;
            const tmp : heap_elt = get_force (i,h);
            h[i] := get_force (right, h);
            h[left] := tmp
          } else skip
        } else skip
    }
   } with (h, result, c)
(* Test a PascaLIGO function which takes another function as an
   argument *)

function foobar (const i : int) : int is
  block {
    function foo (const i : int) : int is i;
    function bar (const f : int -> int) : int is f (i);
  } with bar (foo)

// higher order function with more than one argument

function higher2 (const i : int; const f : int -> int): int is f (i)

function foobar2 (const i : int) : int is
  block {
    function foo2 (const i : int) : int is i
  } with higher2 (i, foo2)

const a : int = 0

function foobar3 (const i : int) : int is
  block {
    function foo2 (const i : int) : int is a+i
  } with higher2 (i, foo2)

function f (const i : int) : int is i

function g (const i : int) : int is f (i)

function foobar4 (const i : int) : int is g (g (i))

function higher3 (const i : int;
                  const f : int -> int;
                  const g : int -> int) : int is f (g (i))

function foobar5 (const i : int) : int is
  block {
    const a : int = 0;
    function foo (const i : int) : int is a+i;
    function goo (const i : int) : int is foo (i)
  } with higher3 (i, foo, goo)

function foobar6 (const i : int) : int -> int is f
type id is int

type id_details is
  record [
    owner: address;
    controller: address;
    profile: bytes;
  ]

type buy is
  record [
    profile: bytes;
    initial_controller: option(address);
  ]

type update_owner is
  record [
    id: id;
    new_owner: address;
  ]

type update_details is
  record [
    id: id;
    new_profile: option(bytes);
    new_controller: option(address);
  ]

type action is
  | Buy of buy
  | Update_owner of update_owner
  | Update_details of update_details
  | Skip of unit

(* The prices kept in storage can be changed by bakers, though they should only be
   adjusted down over time, not up. *)
type storage is
  record [
    identities: big_map (id, id_details);
    next_id: int;
    name_price: tez;
    skip_price: tez;
  ]

(** Preliminary thoughts on ids:

I very much like the simplicity of http://gurno.com/adam/mne/.
5 three letter words means you have a 15 character identity, not actually more
annoying than an IP address and a lot more memorable than the raw digits. This
can be stored as a single integer which is then translated into the corresponding
series of 5 words.

I in general like the idea of having a 'skip' mechanism, but it does need to cost
something so people don't eat up the address space. 256 ^ 5 means you have a lot
of address space, but if people troll by skipping a lot that could be eaten up.
Should probably do some napkin calculations for how expensive skipping needs to
be to deter people from doing it just to chew up address space.
*)

function buy (const parameter : buy; const storage : storage) : list(operation) * storage is
  begin
    if amount = storage.name_price
    then skip
    else failwith("Incorrect amount paid.");
    const profile : bytes = parameter.profile;
    const initial_controller : option(address) = parameter.initial_controller;
    var identities : big_map (id, id_details) := storage.identities;
    const new_id : int = storage.next_id;
    const controller : address =
      case initial_controller of
        Some(addr) -> addr
      | None -> sender
      end;
    const new_id_details: id_details =
      record [
              owner = sender ;
              controller = controller ;
              profile = profile ;
      ];
    identities[new_id] := new_id_details;
  end with ((nil : list(operation)), storage with record [
                              identities = identities;
                              next_id = new_id + 1;
                              ])

function update_owner (const parameter : update_owner; const storage : storage) :
         list(operation) * storage is
  begin
    if (amount =/= 0mutez)
    then
      begin
        failwith("Updating owner doesn't cost anything.");
      end
    else skip;
    const id : int = parameter.id;
    const new_owner : address = parameter.new_owner;
    var identities : big_map (id, id_details) := storage.identities;
    var id_details : id_details :=
      case identities[id] of
        Some(id_details) -> id_details
      | None -> (failwith("This ID does not exist."): id_details)
      end;
    if sender = id_details.owner
    then skip;
    else failwith("You are not the owner of this ID.");
    id_details.owner := new_owner;
    identities[id] := id_details;
  end with ((nil: list(operation)), storage with record [ identities = identities; ])

function update_details (const parameter : update_details; const storage : storage ) :
         list(operation) * storage is
  begin
    if (amount =/= 0mutez)
    then failwith("Updating details doesn't cost anything.")
    else skip;
    const id : int = parameter.id;
    const new_profile : option(bytes) = parameter.new_profile;
    const new_controller : option(address) = parameter.new_controller;
    var identities : big_map (id, id_details) := storage.identities;
    var id_details: id_details :=
      case identities[id] of
        Some(id_details) -> id_details
      | None -> (failwith("This ID does not exist."): id_details)
      end;
    if (sender = id_details.controller) or (sender = id_details.owner)
    then skip;
    else failwith("You are not the owner or controller of this ID.");
    const owner: address = id_details.owner;
    const profile: bytes =
      case new_profile of
        None -> (* Default *) id_details.profile
      | Some(new_profile) -> new_profile
      end;
    const controller: address =
    case new_controller of
      None -> (* Default *) id_details.controller
    | Some(new_controller) -> new_controller
    end;
    id_details.owner := owner;
    id_details.controller := controller;
    id_details.profile := profile;
    identities[id] := id_details;
  end with ((nil: list(operation)), storage with record [ identities = identities; ])

(* Let someone skip the next identity so nobody has to take one that's undesirable *)
function skip_ (const p: unit; const storage: storage) : list(operation) * storage is
  begin
    if amount = storage.skip_price
    then skip
    else failwith("Incorrect amount paid.");
  end with ((nil: list(operation)), storage with record [ next_id = storage.next_id + 1; ])

function main (const action : action; const storage : storage) : list(operation) * storage is
  case action of
  | Buy(b) -> buy (b, storage)
  | Update_owner(uo) -> update_owner (uo, storage)
  | Update_details(ud) -> update_details (ud, storage)
  | Skip(s) -> skip_ (unit, storage)
  end;
function main (const kh: key_hash) : contract (unit) is
  Tezos.implicit_account (kh)
// Test PascaLIGO inclusion statements, see includer.ligo

const foo : int = 144
// Test PascaLIGO inclusion statements, see included.ligo


const bar : int = foo
function main (const i : int) : option (nat) is is_nat (i)
function check_hash_key (const kh1 : key_hash;
                         const k2 : key) : bool * key_hash is
  block {
    var kh2 : key_hash := Crypto.hash_key (k2);
  } with ((kh1 = kh2), kh2)
type storage is record [
  one : map (key_hash, nat);
  two : big_map (key_hash, bool)
]

type return is list (operation) * storage

function main (const a : int; const store : storage) : return is
  ((nil : list (operation)), store)
function f (const x : unit) : unit is Unit

function main (const p : unit ; const s : unit) : unit is f (Unit)
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
    function aggregate (const i : int) : unit is
      block { skip (* r := r + 1 *) } with unit;
    List.iter (aggregate, s)
  } with r

function map_op (const s : list (int)) : list (int) is
  block {
    function increment (const i : int) : int is i+1
  } with List.map (increment, s)
function local_type (var u : unit) : int is block {
	type toto is int;
	var titi : toto := 1;
	titi := titi + 2
} with titi
// Test while loops in PascaLIGO

function counter (var n : nat) : nat is
  block {
    var i : nat := 0n;
    while i < n block {
      i := i + 1n
    }
  } with i

function while_sum (var n : nat) : nat is
  block {
    var i : nat := 0n;
    var r : nat := 0n;
    while i < n block {
      i := i + 1n;
      r := r + i
    }
  } with r

function for_sum (var n : nat) : int is
  block {
    var acc : int := 0;
    for i := 1 to int (n)
      block {
        acc := acc + i
      }
  } with acc

function for_sum_step (var n : nat) : int is
  block {
    var acc : int := 0;
    for i := 1 to int (2n*n) step 2
      block {
        acc := acc + i
      }
  } with acc

function for_collection_list (var nee : unit) : (int * string) is
  block {
    var acc : int := 0;
    var st : string := "to";
    var mylist : list (int) := list [1; 1; 1];
    for x in list mylist
      block {
        acc := acc + x;
        st := st ^ "to"
      }
  } with (acc, st)

function for_collection_set (var nee : unit) : int * string is
  block {
    var acc : int := 0;
    var st : string := "to";
    var myset : set (int) := set [1; 2; 3];
    for x in set myset block {
      acc := acc + x;
      st := st ^ "to"
    }
  } with (acc, st)

function for_collection_if_and_local_var (var nee : unit) : int is
  block {
    var acc : int := 0;
    const theone : int = 1;
    const thetwo : int = 2;
    var myset : set (int) := set [1; 2; 3];
    for x in set myset block {
      if x = theone then acc := acc + x
      else if x = thetwo then acc := acc + thetwo
      else acc := acc + 10
    }
  } with acc

function for_collection_rhs_capture (var nee : unit) : int is
  block {
    var acc : int := 0;
    const mybigint : int = 1000;
    var myset : set (int) := set [1; 2; 3];
    for x in set myset block {
      if x = 1 then acc := acc + mybigint
      else acc := acc + 10
    }
  } with acc

function for_collection_proc_call (var nee : unit) : int is
  block {
    var acc : int := 0;
    var myset : set (int) := set [1; 2; 3];
    for x in set myset block {
      if x = 1 then
        acc := acc + for_collection_rhs_capture (unit)
      else acc := acc + 10
    }
  } with acc

function for_collection_comp_with_acc (var nee : unit) : int is
  block {
    var myint : int := 0;
    var mylist : list (int) := list [1; 10; 15];
    for x in list mylist block {
      if x < myint then skip;
      else myint := myint + 10
    }
  } with myint

function for_collection_with_patches (var nee : unit) : map (string,int) is
  block {
    var myint : int := 12;
    var mylist : list (string) := list ["I"; "am"; "foo"];
    var mymap : map (string,int) := map [];
    for x in list mylist block {
      patch mymap with map [x -> myint]
    }
  } with mymap

function for_collection_empty (var nee : unit) : int is
  block {
    var acc : int := 0;
    var myset : set(int) := set [1; 2; 3];
    for x in set myset block {
      skip
    }
  } with acc

function for_collection_map_kv (var nee : unit) : int * string is
  block {
    var acc : int := 0;
    var st : string := "";
    var mymap : map (string, int) := map ["1" -> 1; "2" -> 2; "3" -> 3];
    for k -> v in map mymap block {
      acc := acc + v;
      st := st ^ k;
    }
  } with (acc, st)

function nested_for_collection (var nee : unit) : int * string is
  block {
    var myint : int := 0;
    var mystoo : string := "";
    var mylist : list(int) := list [1; 2; 3];
    var mymap : map (string, string) := map [" one" -> ","; "two" -> " "];
    for i in list mylist block {
      myint := myint + i;
      var myset : set (string) := set ["1"; "2"; "3"];
      for st in set myset block {
        myint := myint + i;
        mystoo := mystoo ^ st;
        for k -> v in map mymap block {
          mystoo := mystoo ^ k ^ v
        }
      }
    }
  } with (myint, mystoo)

function nested_for_collection_local_var (var nee : unit) : int*string is
  block {
    var myint : int := 0;
    var myst : string := "";
    var mylist : list (int) := list [1; 2; 3];
    for i in list mylist block {
      var myst_loc : string := "";
      myint := myint + i;
      var myset : set (string) := set ["1"; "2"; "3"];
      for st in set myset block {
        myint := myint + i;
        myst_loc := myst_loc ^ st;
      };
      myst := myst_loc ^ myst
    }
  } with (myint, myst)

function dummy (const n : nat) : nat is block {
  while False block { skip }
} with n

function inner_capture_in_conditional_block (var nee : unit) : bool * int is
  block {
    var count : int := 1;
    var ret : bool := False;
    var mylist : list (int) := list [1; 2; 3];
    for it1 in list mylist block {
      for it2 in list mylist block {
        if count = it2 then ret := not (ret) else skip
      };
      count := count + 1
    }
  } with (ret, count)
function shadowing_in_body (var nee : unit) : string is block {
  var st : string := "";
  var list1 : list (string) := list ["to"; "to"];
  for x in list list1 block {
    const x : string = "ta";
    st := st ^ x;
  }
} with st
(* should be "tata" *)

function shadowing_assigned_in_body (var nee : unit) : string is block {
  var st : string := "";
  var list1 : list (string) := list ["to"; "to"];
  for x in list list1 block {
    st := st ^ x;
    var st : string := "ta";
    st := st ^ x;
  }
} with st
(* should be "toto" ??? *)
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
    function increment (const i : int; const j : int) : int is j+1
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
// Test the pattern matching functionality of PascaLIGO

function match_bool (const i : int) : int is
  block {
    var result : int := 23;
    case i = 2 of
      True  -> result := 42
    | False -> result := 0
    end
  } with result

function match_option (const o : option (int)) : int is
  block {
    var result : int := 23;
    case o of
      None -> skip
    | Some (s) -> result := s
    end
  } with result

function match_expr_bool (const i : int) : int is
  case i = 2 of
    True -> 42
  | False -> 0
  end

function match_expr_option (const o : option (int)) : int is
  case o of
    None -> 42
  | Some (s) -> s
  end

function match_expr_list (const l : list (int)) : int is
  case l of
    nil -> -1
  | hd # tl -> hd
  end
// Test michelson insertion in PascaLIGO

function michelson_add (var n : nat * nat ) : nat is block {
  const f : (nat * nat -> nat)= [%Michelson ({| { UNPAIR; ADD } |} : nat *nat -> nat)];
} with f (n)
type inner_storage is michelson_or(int,"one",nat,"two")
type storage is michelson_or (int,"three",inner_storage,"four")

type return is list(operation) * storage

function main (const action : unit; const store : storage) : return is block {
  const foo : storage = (M_right ((M_left(1) : inner_storage)) : storage) ;
} with ((nil : list(operation)), (foo: storage))
type inner_storage is michelson_or(int,"one",nat,"two")
type storage is michelson_or (int,"three",inner_storage,"")

type return is list(operation) * storage

function main (const action : unit; const store : storage) : return is block {
  const foo : storage = (M_right ((M_left(1) : inner_storage)) : storage) ;
} with ((nil : list(operation)), (foo: storage))
type inner_storage is michelson_pair(int,"one",nat,"two")
type storage is michelson_pair (string,"three",inner_storage,"four")

type return is list(operation) * storage

function main (const action : unit; const store : storage) : return is block {
  const foo : storage = ("foo",(1,2n)) ;
} with ((nil : list(operation)), (foo: storage))type inner_storage is michelson_pair(int,"one",nat,"two")
type storage is michelson_pair (string,"three",inner_storage,"")

type return is list(operation) * storage

function main (const action : unit; const store : storage) : return is block {
  const foo : storage = ("foo",(1,2n)) ;
} with ((nil : list(operation)), (foo: storage))
// Test functions with several parameters in PascaLIGO

function ab (const a : int; const b : int) : int is a+b

function abcd (const a : int;
               const b : int;
               const c : int;
               const d : int) : int is a+b+c+d+2

function abcde (const a : int;
                const b : int;
                const c : int;
                const d : int;
                const e : int) : int is c+e+3
// storage type

type threshold is nat
type max_proposal is nat
type max_message_size is nat
type state_hash is bytes
type addr_set is set (address)
type message_store is map (bytes, addr_set)
type proposal_counters is map (address, nat)

type storage is
  record [
    state_hash           : state_hash;
    threshold            : threshold;
    max_proposal         : max_proposal;
    max_message_size     : max_message_size;
    authorized_addresses : addr_set;
    message_store        : message_store;
    proposal_counters    : proposal_counters
  ]

// I/O types

type message is bytes -> list (operation)
type send_pt is message
type withdraw_pt is message
type default_pt is unit

type return is list (operation) * storage

type parameter is
  Send     of send_pt
| Withdraw of withdraw_pt
| Default  of default_pt

function send (const param : send_pt; var s : storage) : return is
  block {
    // check sender against the authorized addresses

    if not Set.mem (Tezos.sender, s.authorized_addresses)
    then failwith("Unauthorized address")
    else skip;

    // check message size against the stored limit

    var message : message := param;
    const packed_msg : bytes = Bytes.pack (message);
    if Bytes.length (packed_msg) > s.max_message_size
    then failwith ("Message size exceed maximum limit")
    else skip;

    (* compute the new set of addresses associated with the message and
       update counters *)

    var new_store : addr_set := set [];

    case map_get (packed_msg, s.message_store) of
      Some (voters) ->
        block {
          (* The message is already stored.
             Increment the counter only if the sender is not already
             associated with the message. *)
          if Set.mem (Tezos.sender, voters)
          then skip
          else s.proposal_counters[Tezos.sender] :=
                 get_force (Tezos.sender, s.proposal_counters) + 1n;
                 new_store := Set.add (Tezos.sender,voters)
        }
    | None ->
        block {
          // the message has never been received before
          s.proposal_counters[sender] :=
             get_force (Tezos.sender, s.proposal_counters) + 1n;
             new_store := set [Tezos.sender]
        }
    end;

    // check sender counters against the maximum number of proposal

    var sender_proposal_counter : nat :=
      get_force (Tezos.sender, s.proposal_counters);

    if sender_proposal_counter > s.max_proposal
    then failwith ("Maximum number of proposal reached")
    else skip;

    // check the threshold

    var ret_ops : list (operation) := nil;

    if Set.cardinal (new_store) >= s.threshold then {
      remove packed_msg from map s.message_store;
      ret_ops := message (s.state_hash);
      // update the state hash
      s.state_hash := Crypto.sha256 (Bytes.concat (s.state_hash, packed_msg));
      // decrement the counters
      for addr -> ctr in map s.proposal_counters block {
        if Set.mem (addr, new_store) then
          s.proposal_counters[addr] := abs (ctr - 1n)
        else skip
      }
    } else s.message_store[packed_msg] := new_store
  } with (ret_ops, s)

function withdraw (const param : withdraw_pt; var s : storage) : return is
  block {
    var message : message := param;
    const packed_msg : bytes = Bytes.pack (message);

    case s.message_store[packed_msg] of
      Some (voters) ->
        block {
          // The message is stored
          const new_set : addr_set = Set.remove (Tezos.sender, voters);

          (* Decrement the counter only if the sender was already
             associated with the message *)

          if Set.cardinal (voters) =/= Set.cardinal (new_set)
          then s.proposal_counters[Tezos.sender] :=
                 abs (get_force (Tezos.sender, s.proposal_counters) - 1n)
          else skip;

          (* If the message is left without any associated addresses,
             remove the corresponding message_store field *)

          if Set.cardinal (new_set) = 0n
          then remove packed_msg from map s.message_store
          else s.message_store[packed_msg] := new_set
        }
    | None -> skip
    end // The message is not stored, ignore.
  } with ((nil : list (operation)), s)

function default (const p : default_pt; const s : storage) : return is
    ((nil : list (operation)), s)

function main (const param : parameter; const s : storage) : return  is
  case param of
    (* Propagate message p if the number of authorized addresses having
       voted for the same message p equals the threshold. *)
    | Send (p) -> send (p, s)

    (* Withraw vote for message p *)
    | Withdraw (p) -> withdraw (p, s)

    (* Use this action to transfer tez to the contract *)
    | Default (p) -> default (p, s)
  end
// storage type

type counter is nat
type threshold is nat
type authorized_keys is list (key)
type id is string

type storage is
  record [
    id        : id;
    counter   : counter;
    threshold : threshold;
    auth      : authorized_keys
  ]

// I/O types

type message is unit -> list (operation)

type signatures is list (key_hash * signature)

type check_message_pt is
  record [
    counter    : counter;
    message    : message;
    signatures : signatures
  ]

type return is list (operation) * storage

type parameter is CheckMessage of check_message_pt

function check_message (const param : check_message_pt;
                        var s : storage) : return is block {
  var message : message := param.message;

  if param.counter =/= s.counter then
    failwith ("Counters does not match")
  else {
    const packed_payload : bytes =
      Bytes.pack ((message, param.counter, s.id, Tezos.chain_id));
    var valid : nat := 0n;

    var keys : authorized_keys := s.auth;
    for pkh_sig in list param.signatures block {
      case keys of
        nil -> skip
      | key # tl -> block {
          keys := tl;
          if pkh_sig.0 = Crypto.hash_key (key) then
            if Crypto.check (key, pkh_sig.1, packed_payload)
            then valid := valid + 1n
            else failwith ("Invalid signature")
          else skip
        }
      end
    };

    if valid < s.threshold then
      failwith ("Not enough signatures passed the check")
    else s.counter := s.counter + 1n
  }
} with (message (unit), s)

function main (const param : parameter; const s : storage) : return is
  case param of CheckMessage (p) -> check_message (p,s) end
// Test the option type in PascaLIGO

type foobar is option (int)

const s : foobar = Some (42)
const n : foobar = None

function assign (var m : int) : foobar is
  block {
    var coco : foobar := None;
    coco := Some (m);
    coco := (None : foobar); //temporary annotation added until type inference
  } with coco
function foo (const input : int) : int is input + 42

function main (const i : int) : int is i + foo (i)
function foo (const input : int) : int is input + 23

function bar (const input : int) : int is input + 51

function main (const i : int) : int is foo (i) + bar (i)
// Test record type in PascaLIGO

type foobar is record [foo : int; bar : int]

const fb : foobar = record [foo = 0; bar = 0]

type abc is record [a : int; b : int; c : int]

const abc : abc = record [a = 42; b = 142; c = 242]

const a : int = abc.a
const b : int = abc.b
const c : int = abc.c

function projection (const r : foobar) : int is r.foo + r.bar

function modify (var r : foobar) : foobar is
  block {
    r.foo := 256
  } with r

function modify_abc (var r : abc) : abc is
  block {
    const c : int = 42;
    r := r with record [b=2048; c=c]
  } with r

type big_record is record [a : int; b : int; c : int; d : int; e : int]

const br : big_record =
  record [a = 23; b = 23; c = 23; d = 23; e = 23]

type double_record is record [inner : abc]

function modify_inner (var r : double_record) : double_record is
  block {
    r := r with record [inner.b = 2048]
  } with r
// Test while loops in PascaLIGO

recursive function sum (const n : int; const acc: int) : int is
  if n<1 then acc else sum(n-1,acc+n)

recursive function fibo (const n: int; const n_1: int; const n_0 :int) : int is
  if n<2 then n_1 else fibo(n-1,n_1+n_0,n_1)
function foo (const p : unit) : int is 0

function main (const p : unit; const s : int) : list (operation) * int is
  ((nil : list (operation)), foo (unit))

function foo (const p : unit) : int is 1
// storage type
type storage_t is address

// entry points parameter types
type change_addr_pt is address

type message_t is list (operation)
type pass_message_pt is unit -> message_t

type contract_return_t is list (operation) * storage_t

type entry_point_t is
  Change_address of change_addr_pt
| Pass_message   of pass_message_pt

function change_address (const param : change_addr_pt;
                         const s : storage_t) : contract_return_t is
  block {
    if sender =/= s then failwith ("Unauthorized sender")
    else skip
  } with ((nil : list (operation)), param)

function pass_message (const param: pass_message_pt;
                       const s : storage_t ) : contract_return_t is
  block {
    if sender =/= s then failwith("Unauthorized sender") else skip;
    var message : pass_message_pt := param
  } with (param (unit), s)

function main (const param : entry_point_t; const s : storage_t) :
  contract_return_t is
  case param of
    Change_address (p) -> change_address (p,s)
  | Pass_message (p)   -> pass_message (p,s)
  end
function main (const p : unit) : address is Tezos.self_address
type parameter is nat
type storage is int
type return is list (operation) * storage


function main (const p : parameter; const s : storage) : return is
  block {
    const self_contract: contract(parameter) = Tezos.self("%default") ;
  }
  with ((nil: list(operation)), s)type parameter is Default | Toto of int
type storage is nat
type return is list (operation) * storage


function main (const p : parameter; const s : storage) : return is
  block {
    // const v : string = "%toto" ;
    const self_contract: contract(int) = Tezos.self("%toto") ;
    const op : operation = Tezos.transaction (2, 300tz, self_contract) ;
  }
  with (list [op], s)type parameter is int
type storage is nat
type return is list (operation) * storage


function main (const p : parameter; const s : storage) : return is
  block {
    const self_contract: contract(int) = Tezos.self("%default") ;
    const op : operation = Tezos.transaction (2, 300tz, self_contract) ;
  }
  with (list [op], s)// Test set iteration in PascaLIGO

function iter_op (const s : set (int)) : int is
  block {
    var r : int := 0;
    function aggregate (const i : int) : unit is
      block {
        skip
      } with unit;
    set_iter (aggregate, s)
  } with r // ALWAYS RETURNS 0

function fold_op (const s : set (int)) : int is
  block {
    function aggregate (const i : int; const j : int) : int is
      i + j
  } with set_fold (aggregate, s, 15)
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
function main (const p : key_hash) : list (operation) is
  block {
    const unused : operation = set_delegate (Some (p));
    const dummy : list (operation) = nil
  } with dummy
function foo (const i : int) : int is
  block {
    function bar (const i : int) : int is i
  } with bar (0)
//Test simple_access in PascaLIGO

type tpi is int * int

type rpi is record [x : int; y : int]

type mpi is map (string, int)

function main (const toto : tpi) : int is
  block {
    var a : tpi := toto;
    var b : rpi := record [x=0; y=1];
    var m : mpi := map ["y" -> 1];
    a.0 := 2;
    b.x := a.0;
    m["x"] := b.x
  } with
      case m["x"] of
        Some (s) -> s
      | None -> 42
      end
const s : string = "toto"
const x : string =  s ^ "bar"
const y : string =  "foo" ^ x
const v : string = {|deadbeef|}
function concat_op (const s : string) : string is
  string_concat (s, "toto")

function slice_op (const s : string) : string is
  string_slice (1n, 2n, s)
type action is
  Increment of int
| Decrement of int

type storage is int

type return is list (operation) * storage

function main (const p : action; const s : int) : return is
  ((nil : list (operation)),
   case p of
     Increment (n) -> s + n
   | Decrement (n) -> s - n
   end)
const add_tez : tez = 21mutez + 0.000_021tez

const sub_tez : tez = 21mutez - 20mutez

(* This is not enough. *)

const not_enough_tez : tez = 461_168_601_842_738_7903mutez

const nat_mul_tez : tez = 1n * 100mutez
const tez_mul_nat : tez = 100mutez * 10n

const tez_div_tez1 : nat = 100mutez / 1mutez
const tez_div_tez2 : nat = 100mutez / 90mutez
const tez_div_tez3 : nat = 100mutez / 110mutez

const tez_mod_tez1 : tez = 100mutez mod 1mutez
const tez_mod_tez2 : tez = 100mutez mod 90mutez
const tez_mod_tez3 : tez = 100mutez mod 110mutez
type storage_t is timestamp

type message_t is unit -> list (operation)
type default_pt is unit
type call_pt is message_t
type contract_return_t is list (operation) * storage_t

type entry_point_t is
| Call of call_pt
| Default of default_pt

function call (const p : call_pt; const s : storage_t) : contract_return_t is
  block {
    if s >= now then failwith ("Contract is still time locked") else skip;
    const message : message_t = p;
    const ret_ops : list (operation) = message (unit)
  } with (ret_ops, s)

function default (const p : default_pt; const s : storage_t) :
  contract_return_t is
  ((nil : list (operation)), s)

function main(const param : entry_point_t; const s : storage_t) :
  contract_return_t is
  case param of
    Call    (p) -> call (p,s)
  | Default (p) -> default (p,s)
  end
type storage_ is timestamp

function main (const p : unit; const s : storage_) :
  list (operation) * storage_ is ((nil: list (operation)), now)
type toto is record [a : nat; b : nat]

const foo : int = 3
type abc is int * int * int

function projection_abc (const tpl : abc) : int is tpl.1

function modify_abc (var tpl : abc) : abc is
  block {
    tpl.1 := 2048
  } with tpl

type foobar is int * int

const fb : foobar = (0,0)

function projection (const tpl : foobar) : int is tpl.0 + tpl.1

type big_tuple is int * int * int * int * int * int * int * int * int * int * int * int

const br : big_tuple = (0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11)

function update (var tpl : big_tuple) : big_tuple is
  block {
    tpl.11 := 2048
  } with tpl
type toto is int

const foo : toto = 23
const u : unit = unit
type foobar is
  Foo of int
| Bar of bool
| Kee of nat

function fb (const p : foobar) : int is
  case p of
    Foo (n) -> n
  | Bar (t) -> 42
  | Kee (n) -> 23
  end
type foobar is
  Foo of int
| Bar of bool
| Kee of nat

const foo : foobar = Foo (42)

const bar : foobar = Bar (True)

const kee : foobar = Kee (23n)
function main (const p : int; const s : int) : list (operation) * int is
  ((nil : list (operation)), s + 1)
// variant defining entrypoints

type action is
  Increment of int
| Decrement of int

type return is list (operation) * int

function add (const a : int; const b : int) : int is a + b

function subtract (const a : int; const b : int) : int is a - b

// main function routing the flow based on the action provided

function main (const p : action; const s : int) : return is
  ((nil : list (operation)),
    case p of
      Increment (n) -> add (s, n)
    | Decrement (n) -> subtract (s, n)
    end)
