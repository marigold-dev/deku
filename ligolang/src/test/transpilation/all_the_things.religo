type tokens = big_map (address, nat)
type allowances = big_map ((address, address), nat) /* (sender,account) -> value */

type storage = {
  tokens      : tokens,
  allowances  : allowances,
  total_amount : nat,
}

type transfer = {
	address_from : address,
	address_to   : address,
	value        : nat,
}

type approve = {
	spender : address,
	value   : nat,
}

type getAllowance = {
	owner    : address,
	spender  : address,
	callback : contract (nat),
}

type getBalance = {
	owner    : address,
	callback : contract (nat),
}

type getTotalSupply = {
	callback : contract (nat),
}

type action =
|	Transfer       ( transfer )
|	Approve        ( approve )
|	GetAllowance   ( getAllowance )
|	GetBalance     ( getBalance )
|	GetTotalSupply ( getTotalSupply )

let transfer = ((p,s) : (transfer, storage)) : (list (operation), storage) => {
   let new_allowances =   
		if (Tezos.sender == p.address_from) { s.allowances; }
		else {
			let authorized_value = switch (Big_map.find_opt ((Tezos.sender,p.address_from), s.allowances)) {
			|	Some value => value
			|	None       => 0n
			};
			if (authorized_value < p.value) { (failwith ("Not Enough Allowance") : allowances); }
			else { Big_map.update ((Tezos.sender,p.address_from), (Some (abs(authorized_value - p.value))), s.allowances); };
		};
	let sender_balance = switch (Big_map.find_opt (p.address_from, s.tokens)) {
	|	Some value => value
	|	None       => 0n
	};
	if (sender_balance < p.value) { (failwith ("Not Enough Balance") : (list (operation), storage)); }
	else {
		let new_tokens = Big_map.update (p.address_from, (Some (abs(sender_balance - p.value))), s.tokens);
		let receiver_balance = switch (Big_map.find_opt (p.address_to, s.tokens)) {
		|	Some value => value
		|	None       => 0n
		};
		let new_tokens = Big_map.update (p.address_to, (Some (receiver_balance + p.value)), new_tokens);
		(([]: list (operation)), { ...s,tokens:new_tokens, allowances:new_allowances});
	};
};

let approve = ((p,s) : (approve, storage)) : (list (operation), storage) => {
	let previous_value = switch (Big_map.find_opt ((p.spender, Tezos.sender), s.allowances)){
	|	Some value => value
	|	None => 0n
	};
	if (previous_value > 0n && p.value > 0n)
	{ (failwith ("Unsafe Allowance Change") : (list (operation), storage)); }
	else {
		let new_allowances = Big_map.update ((p.spender, Tezos.sender), (Some (p.value)), s.allowances);
		(([] : list (operation)), { ...s, allowances : new_allowances});
	};
};

let getAllowance = ((p,s) : (getAllowance, storage)) : (list (operation), storage) => {
	let value = switch (Big_map.find_opt ((p.owner, p.spender), s.allowances)) {
	|	Some value => value
	|	None => 0n
	};
	let op = Tezos.transaction (value, 0mutez, p.callback);
	([op],s)
};

let getBalance = ((p,s) : (getBalance, storage)) : (list (operation), storage) => {
	let value = switch (Big_map.find_opt (p.owner, s.tokens)) {
	|	Some value => value
	|	None => 0n
	};
	let op = Tezos.transaction (value, 0mutez, p.callback);
	([op],s)
};

let getTotalSupply = ((p,s) : (getTotalSupply, storage)) : (list (operation), storage) => {
  let total = s.total_amount;
  let op    = Tezos.transaction (total, 0mutez, p.callback);
  ([op],s)
};


let main = ((a,s): (action, storage)) =>  
 	switch a {
   |	Transfer p => transfer ((p,s))
	|	Approve  p => approve ((p,s))
	|	GetAllowance p => getAllowance ((p,s))
	|  GetBalance p => getBalance ((p,s))
	|	GetTotalSupply p => getTotalSupply ((p,s))
	};
let main = (p : key_hash) : address => {
  let c : contract (unit) = Tezos.implicit_account (p);
  Tezos.address(c);
};
let check_ = (p : unit) : int =>
  if (Tezos.amount == 100tez) { 42; } else { 0; };
/* Test ReasonLIGO arithmetic operators */

let mod_op   = (n : int) : nat => n mod 42;
let plus_op  = (n : int) : int => n + 42;
let minus_op = (n : int) : int => n - 42;
let times_op = (n : int) : int => n * 42;
let div_op   = (n : int) : int => n / 2;
let neg_op   = (n : int) : int => - n;
let foo      = (n : int) : int => n + 10;
let neg_op_2 = (b : int) : int => -foo(b);
let ediv_op  = (n : int) : option ((int, nat)) => ediv (n,2) 
let main = (p: bool,s: unit) => {
  let u : unit = assert (p);
  ([]: list (operation), s);
};

let some = (o : option (unit)) => {
  assert_some (o)
};
[@inline]
let x = 1;

[@inline]
let foo = (a : int) : int => {
  [@inline]
  let test = 2 + a;
  test;
};

[@inline][@other]
let y = 1;

let bar = (b : int) : int => {
  [@inline][@foo][@bar]
  let test = (z : int) => 2 + b + z;
  test (b);
};
let main = (parameter : int, storage : address) =>
  ([] : list (operation), "KT1badaddr" : address);
/**

This test makes sure that the balance is accessible in ReasonLIGO.
It's there to detect a regression of: https://gitlab.com/ligolang/ligo/issues/61

Which results in this error when you attempt to compile this contract:

generated. unrecognized constant: {"constant":"BALANCE","location":"generated"}


*/

type storage = tez;

let main2 = (p : unit, s: storage) =>
  ([]: list (operation), Tezos.balance);

let main = (x : (unit, storage)) => main2 (x[0], x[1]);
type toto = int;

let foo : toto = 42 + 127;
type foo = big_map(int, int);

let set2 = (n : int, m : foo) : foo => Big_map.update (23, Some (n), m);

let set_ = (x : (int, foo)) : foo => set2 (x[0], x[1]);

let add = ((n,m) : (int, foo)) : foo => Big_map.add (23, n, m);

let rm = (m : foo) : foo => Big_map.remove (42, m);

let gf = (m : foo) : int => Big_map.find (23, m);

let get = (m : foo) : option (int) => Big_map.find_opt (42, m);

let empty_map : foo = Big_map.empty;

let map1 : foo = Big_map.literal ([(23,0), (42,0)]);

let mutimaps = (m: foo, n: foo): foo => {
  let bar : foo = Big_map.update (42, Some (0), m);
  Big_map.update (42, get (bar), n);
};
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
// Test ReasonLIGO boolean operators

let or_true   = (b : bool) : bool => b || true;
let or_false  = (b : bool) : bool => b || false;
let and_true  = (b : bool) : bool => b && true;
let and_false = (b : bool) : bool => b && false;
let not_bool  = (b : bool) : bool => !b;
let concat_op = (s: bytes): bytes => Bytes.concat(s, 0x7070);

let slice_op = (s: bytes): bytes => Bytes.slice(1n, 2n, s);

let hasherman = (s: bytes): bytes => Crypto.sha256(s);
let id_string = (p : string) : option(string) => {
  let packed : bytes = Bytes.pack (p);
  ((Bytes.unpack (packed)) : option (string));
};

let id_int = (p : int) : option (int) => {
  let packed : bytes = Bytes.pack (p);
  ((Bytes.unpack (packed)) : option (int));
};

let id_address = (p : address) : option (address) => {
  let packed : bytes = Bytes.pack (p);
  ((Bytes.unpack (packed)) : option (address));
};
let check_signature = (param : (key, signature, bytes)) : bool => {
  let pk, signed, msg = param;
  Crypto.check (pk, signed, msg);
};
/* Test whether closures retain values in ReasonLIGO */

let test = (k : int) : int => {
  let j : int = k + 5;
  let close : (int => int) = (i : int) => i + j;

  let j : int = 20; /* Shadow original variable */
  close (20);
};
let main = (i : int) =>
  if (((i == 2) : bool)) { (42 : int); } else { (0 : int); };
/* TODO : make a test using mutation, not shadowing */

let main = (i: int) => {
  let result = 0;
  if (i == 2) {
    let result = 42;
    result;
  } else {
    let result = 0;
    result;
  };
};
/* Test conditional in ReasonLIGO */

let main = (i : int) => if (i == 2) { 42; } else { 0; };
type storage = int;

let main = ((p, s) : (int, storage)) : (list (operation), storage) =>
  ([] : list (operation), p + s);
let hasherman512    = (s : bytes) => Crypto.sha512 (s);
let hasherman_blake = (s : bytes) => Crypto.blake2b (s);
type tokenId = nat;
type tokenOwner = address;
type tokenAmount = nat;
type transferContents = {
    to_: tokenOwner,
    token_id: tokenId,
    amount: tokenAmount
};
type transfer = {
    from_: tokenOwner,
    txs: list(transferContents)
};
type transferContentsMichelson = michelson_pair_right_comb(transferContents);
type transferAuxiliary = {
    from_: tokenOwner,
    txs: list(transferContentsMichelson)
};
type transferMichelson = michelson_pair_right_comb(transferAuxiliary);
type transferParameter = list(transferMichelson);
type parameter = 
| Transfer(transferParameter)
type storage = big_map(tokenId, tokenOwner);
type entrypointParameter = (parameter, storage);
type entrypointReturn = (list(operation), storage);
let errorTokenUndefined = "TOKEN_UNDEFINED";
let errorNotOwner = "NOT_OWNER";
let errorInsufficientBalance = "INSUFFICIENT_BALANCE";
type transferContentsIteratorAccumulator = (storage, tokenOwner);
let allowOnlyOwnTransfer = (from: tokenOwner): unit => {
    if (from != Tezos.sender) {
        failwith(errorNotOwner)
    } else { (); }
}
let transfer = ((transferParameter, storage): (transferParameter, storage)): entrypointReturn => {
    let storage = List.fold(transferIterator, transferParameter, storage);
    (([]: list(operation)), storage);
};
let main = ((parameter, storage): entrypointParameter): entrypointReturn => {
    switch (parameter) {
        | Transfer(transferParameter) => transfer((transferParameter, storage))
    }
}
type foo =
| Bar (int)
| Baz;

let main = (f : foo) : int =>
  switch (f) {
  | Bar (i) => i
  | Baz     => (-1)
  };
/* Test boolean comparison in ReasonLIGO */

let main = ((a, b) : (bool, bool)) =>
  if (a == b) { 999; } else { 1; };
type toto = int
type a = (string)
type a = (string, int)
type foo = list(int) => int
type foo = (int) => int
type a = ((((string, int))))
type b = string => int
type b = (string) => int
type b = string => (int, string) => int
type b = string => (int) => int
type b = (string, int) => int
type b = (((string, int))) => int
type c = (string) => (int) => string
type c = (string => int) => int
type c = ((string) => int) => int

// // // // // // // /* not supported yet: type c = ((string, int) => int, int) => int */
// // // // // // // /* not supported yet: type d = Foo (int, string) */
type d = 
| Foo(int)
| Bar((int, string, bool))
type d = 
| Foo((string) => int)
| Bar((int, string, bool) => string)
let e:string = switch (a) {
  | Pleh => "x"
  | Dog (foo) => "a"
};
let f = foo => foo
let f = (foo:string) => foo
let f = (((foo:string))) => foo
let f = (foo:string):int => foo
let f = (foo:string, g: int):int => foo
let f = (foo:string, g: int):(int, string) => foo
let f = foo:(int, string) => foo
let a = f: string => f
let g = (p: key_hash) : list (operation) => 2
let foo: int => int = (i: int) => i;

let foo : toto = 42 + 127;
let main = (kh : key_hash) : contract (unit) =>
  Tezos.implicit_account (kh);
let store = switch (action) {
| Increment (n) => store + n
| Decrement (n) => store - n
};
let arguments_type_def_inline = (b: (int) => int) => b (5, 3);
let aux = ((vk, pkh_sig) :
                 ((intx, intx), (int2, int2)))
                 : (nat, authorized_keys) => 222
let main = ((action, s) : (parameter, storage)) : return => {
  (([]: list(operation), storage));
};
type parameter =
| Increment (nat)
| Decrement (nat)
| Reset;

type storage = unit;

type return = (list (operation), storage);

let dest : address = ("KT19wgxcuXG9VH4Af5Tpm1vqEKdaMFpznXT3" : address);

let proxy = ((action, store): (parameter, storage)) : return => {
  let counter : contract (parameter) =
    switch (a : b) {
    | Some2 (contract) => contract;
    | None2 => (failwith ("Contract not found.") : contract (parameter));
    };
    counter
//   /* Reuse the parameter in the subsequent
//      transaction or use another one, `mock_param`. */
//   let mock_param : parameter = Increment (5n);
//   let op : operation = Tezos.transaction (action, 0tez, counter);
//   ([op], store)
};type storage = unit;

let main = (p: unit, s : storage) =>
  if (true) { failwith("This contract always fails"); };
/* Test use of multiple subroutines in a ReasonLIGO function */

let foo    = (i : int) : int => i + 20;
let bar    = (i : int) : int => i + 50;
let foobar = (i : int) : int => foo (i) + bar (i);
/* Test a function which takes another function as an argument */

let foobar = (i : int): int => {
  let foo: int => int = (i: int) => i;
  let bar: ((int => int) => int) = (f : (int => int)) => f (i);
  bar (foo);
};

/* higher order function with more than one argument */

let higher2 = (i : int, f : (int => int)) : int => {
  let ii : int = f (i);
  ii;
};

let foobar2 = (i : int) : int => {
  let foo2 : int => int = (i : int) => i;
  higher2 (i, foo2);
};

let a : int = 0;

let foobar3 = (i : int) : int => {
  let foo2: int => int = (i : int) => a + i;
  higher2 (i, foo2);
};

let f = (i : int) : int => i;

let g = (i : int) : int => f (i);

let foobar4 = (i : int) : int => g (g (i));

let higher3 = (i : int, f : (int => int), g : (int => int)) : int => {
  let ii : int = f (g (i));
  ii;
};

let foobar5 = (i : int) : int => {
  let a : int = 0;
  let foo : int => int = (i : int) => a + i;
  let goo : int => int = (i : int) => foo (i);
  higher3 (i, foo, goo);
};
type id = int

type id_details = {
  owner: address,
  controller: address,
  profile: bytes,
}

type buy = {
  profile: bytes,
  initial_controller: option(address),
}

type update_owner = {
  id: id,
  new_owner: address,
}

type update_details = {
  id: id,
  new_profile: option(bytes),
  new_controller: option(address),
}

type action =
| Buy(buy)
| Update_owner(update_owner)
| Update_details(update_details)
| Skip(unit)

/* The prices kept in storage can be changed by bakers, though they should only be
   adjusted down over time, not up. */
type storage = {
  identities: big_map (id, id_details),
  next_id: int,
  name_price: tez,
  skip_price: tez,
}

/** Preliminary thoughts on ids:

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
*/

let buy = ((parameter, storage): (buy, storage)) : (list(operation), storage) => {
  let void: unit =
    if (amount == storage.name_price) { (); }
    else { failwith("Incorrect amount paid."); };
  let profile = parameter.profile;
  let initial_controller = parameter.initial_controller;
  let identities = storage.identities;
  let new_id = storage.next_id;
  let controller: address =
    switch (initial_controller) {
      | Some(addr) => addr
      | None => sender
    };
  let new_id_details: id_details = {
    owner : sender,
    controller : controller,
    profile : profile,
  };
  let updated_identities: big_map (id, id_details) =
    Big_map.update(new_id, Some(new_id_details), identities);
  (([]: list(operation)), { ...storage,
                           identities : updated_identities,
                           next_id : new_id + 1,
                        });
  };

let update_owner = ((parameter, storage): (update_owner, storage)) : (list(operation), storage) => {
  let void: unit =
    if (amount != 0mutez) {
      failwith("Updating owner doesn't cost anything.");
    }
    else { (); };
  let id : int = parameter.id;
  let new_owner = parameter.new_owner;
  let identities = storage.identities;
  let current_id_details: id_details =
    switch (Big_map.find_opt(id, identities)) {
      | Some(id_details) => id_details
      | None => (failwith("This ID does not exist."): id_details)
    };
  let u: unit =
    if (sender == current_id_details.owner) { (); }
    else { failwith("You are not the owner of this ID."); };
  let updated_id_details: id_details = {
    owner : new_owner,
    controller : current_id_details.controller,
    profile : current_id_details.profile,
  };
  let updated_identities = Big_map.update(id, (Some updated_id_details), identities);
  (([]: list(operation)), { ...storage, identities : updated_identities });
  };

let update_details = ((parameter, storage): (update_details, storage)) :
                   (list(operation), storage) => {
  let void : unit = 
    if (amount != 0mutez) {
      failwith("Updating details doesn't cost anything.");
    }
    else { (); };
  let id = parameter.id;
  let new_profile = parameter.new_profile;
  let new_controller = parameter.new_controller;
  let identities = storage.identities;
  let current_id_details: id_details =
    switch (Big_map.find_opt(id, identities)) {
      | Some(id_details) => id_details
      | None => (failwith("This ID does not exist."): id_details)
    };
  let u: unit =
    if ((sender != current_id_details.controller) &&
        (sender != current_id_details.owner)) {
      failwith ("You are not the owner or controller of this ID.")
    }
    else { (); };
  let owner: address = current_id_details.owner;
  let profile: bytes =
    switch (new_profile) {
      | None => /* Default */ current_id_details.profile
      | Some(new_profile) => new_profile
    };
  let controller: address =
    switch (new_controller) {
      | None => /* Default */ current_id_details.controller
      | Some new_controller => new_controller
    };
  let updated_id_details: id_details = {
    owner : owner,
    controller : controller,
    profile : profile,
  };
  let updated_identities: big_map (id, id_details) =
    Big_map.update(id, (Some updated_id_details), identities);
  (([]: list(operation)), { ...storage, identities : updated_identities });
  };

/* Let someone skip the next identity so nobody has to take one that's undesirable */
let skip = ((p,storage): (unit, storage)) => {
  let void : unit =
    if (amount != storage.skip_price) {
      failwith("Incorrect amount paid.");
    }
    else { (); };
  (([]: list(operation)), { ...storage, next_id : storage.next_id + 1 });
  };

let main = ((action, storage): (action, storage)) : (list(operation), storage) => {
  switch (action) {
    | Buy(b) => buy((b, storage))
    | Update_owner(uo) => update_owner((uo, storage))
    | Update_details ud => update_details((ud, storage))
    | Skip s => skip(((), storage))
  };
};
let main = (kh : key_hash) : contract (unit) =>
  Tezos.implicit_account (kh);
// Demonstrate ReasonLIGO inclusion statements, see includer.religo

let foo : int = 144;
// Demonstrate ReasonLIGO inclusion statements, see included.religo

let bar : int = foo;
let main = (i : int): option (nat) => is_nat (i);
let check_hash_key = (kh1_k2 : (key_hash, key)) : (bool, key_hash) => {
  let kh1, k2 = kh1_k2;
  let kh2 : key_hash = Crypto.hash_key (k2);
  ((kh1 == kh2), kh2)
};
type storage = unit;

/* not supported yet
   let%entry main (p:unit) storage =
     (fun x -> ()) ()
   */

let main = ((p,s) : (unit, storage)) : unit =>
  (((useless : unit)) => ()) ();
type storage = unit;

/* Not supported yet:
   let main (p:unit) storage = (fun x -> ()) ()
   */

let main = ((a, s) : (unit, storage)) : unit =>
  ((f : (unit => unit)) => f ()) ((useless : unit) => unit);
// type foo = { a :int , b : nat }
// type bar = { c : int * nat ; d : foo }

let t1 =
  let (a,b,c,d) = (1,2,3,4) ;
  a

let t2 : string =
  let (a,(b,c),(d,(e,f))) = (1,(2,3),(4,(5,"7"))) ;
  f

let t3 =
  let (a,(b,(c,(d,e,f)))) = (1,(2,(3n,(4,5,"7")))) ;
  (a+b,c,f)

let t4 =
  let (a,(b,(c,(d,(e,f))),g)) = (1,(1n,(1,(1n,(1,1n))),1)) ;
  (a+c+e+g , b + d + f)

// let t5 : nat =
//   let { a = a , b = b } = { a : 1 , b : 1n } ;
//   b

// let t6 =
//   let x : foo =  { a = 1 ; b = 2n } in
//   let ({ a = a ; b = b },(c,d)) = (x,(1,1)) in
//   (a + c + d, b)

// let t7 =
//   let ( { c = (a,b) ; d = { a = c ; b = d } } ) = { c = ( 1 , 2n) ; d = { a = 1 ; b = 1n } } in
//   ( a + c , b + d )

// let t8 =
//   let (x, (y, { a = a ; b = b })) = (1, (1n, {a = 1 ; b = 1n})) in
//   (x + a , y + b)/* Simple test of binding multiple values */

let ((x : int), (y : int)) = (1,2);

let main = (p : unit): int => x + y;

let ((x : int), (y : int)) = (3,3);

let main_paren = (p : unit): int => x + y;

let foobar : (int, int) = (23, 42);

let ((foo : int), (bar : int)) = foobar;

let non_tuple_rhs = (p : unit) : int => foo + bar;
type storage = (int, int);

let main = (n : (int, storage)) : (list (operation), storage) => {
  let x : (int, int) = {
    let x : int = 7;
    (x + n[0], n[1][0] + n[1][1]);
  };
  ([]: list (operation), x);
};

let f0 = (a: string) => true
let f1 = (a: string) => true
let f2 = (a: string) => true

let letin_nesting = (_: unit) => {
  let s = "test";
  let p0 = f0(s);
  assert(p0);
  let p1 = f1(s);
  assert(p1);
  let p2 = f2(s);
  assert(p2);
  s
}

let letin_nesting2 = (x: int) => {
  let y = 2;
  let z = 3;
  x + y + z
}

let x = {
  let (_, (x, _)) = (1n, (2n, 3n));
  x
}
type storage = (int, list (int));

type parameter = list (int);

type return = (list (operation), storage);

let x : list (int) = [];
let y : list (int) = [3, 4, 5];
let z : list (int) = [2, ...y];

let main = ((action, s) : (parameter, storage)) : return => {
  let storage =
    switch (action) {
    | [] => s
    | [hd, ...tl] => (s[0] + hd, tl)
    };
  ([]: list(operation), storage);
};

let size_ = (s : list (int)) : nat => List.length (s);

let fold_op = (s : list (int)) : int => {
  let aggregate = (t: (int, int)) => t[0] + t[1];
  List.fold (aggregate, s, 10);
};

let map_op = (s : list (int)) : list (int) =>
  List.map ((cur : int) => cur + 1, s);

let iter_op = (s : list (int)) : unit => {
  let do_nothing = (useless : int) => unit;
  List.iter (do_nothing, s);
};
let local_type = ( u : unit) : int => {
	type toto = int;
	let titi : toto = 1;
	let titi = titi + 2;
	titi
};
/* Test loops in ReasonLIGO */

let rec aux_simple = (i : int) : int =>
  if (i < 100) { aux_simple (i + 1); } else { i; };

let counter_simple = (n : int) : int => aux_simple (n);

type sum_aggregator = {
  counter : int,
  sum     : int,
};

let counter = (n : int) : int => {
  let initial : sum_aggregator = {counter: 0, sum: 0};
  let rec aggregate = (prev : sum_aggregator):int =>
    if (prev.counter <= n) {
      aggregate ({counter : prev.counter + 1,
                    sum : prev.counter + prev.sum});
    } else {
      prev.sum;
    };
  aggregate (initial);
};

let rec aux_nest = (prev : sum_aggregator) : sum_aggregator =>
  if (prev.counter < 100) {
    let sum : int =
      prev.sum + aux_simple (prev.counter);
    aux_nest ({counter: prev.counter + 1,
                  sum: sum});
  } else {
    ({counter: prev.counter, sum: prev.sum});
  };

let counter_nest = (n : int) : int => {
  let initial : sum_aggregator = {counter: 0, sum: 0};
  let out : sum_aggregator = aux_nest (initial);
  out.sum;
};
type foobar = map (int, int);

let empty_map: foobar = Map.empty;

let map1 : foobar =
  Map.literal ([(144, 23), (51, 23), (42, 23), (120, 23), (421, 23)]);

let map2 : foobar = Map.literal ([(23, 0), (42, 0)]);

let set_ = (n: int, m: foobar) : foobar => Map.update (23, Some (n), m);

let add = (n: int, m: foobar) : foobar => Map.add (23, n, m);

let rm = (m: foobar) : foobar => Map.remove (42, m);

/* Dummy test so that we can add the same test for PascaLIGO */

let patch_ = (m : foobar) : foobar =>
  Map.literal ([(0, 5), (1, 6), (2, 7)]);

/* Second dummy test, see above */

let patch_empty = (m : foobar) : foobar =>
  Map.literal ([(0, 0), (1, 1), (2, 2)]);

/* Third dummy test, see above */

let patch_deep = (m : (foobar, nat)) : (foobar, nat) => (
  Map.literal([(0, 0), (1, 9), (2, 2)]),
  10n
);

let size_ = (m : foobar) : nat => Map.size (m);

let get = (m: foobar): option(int) => Map.find_opt (42, m);
let get_ = (m: foobar): option(int) => Map.find_opt(42, m);

let mem = (km: (int, foobar)): bool => Map.mem (km[0], km[1]);

let iter_op = (m: foobar): unit => {
  let assert_eq = (i: int, j: int) => assert (i == j);
  Map.iter (assert_eq, m);
};

let map_op = (m: foobar) : foobar => {
  let increment = (z: int, j: int) => j + 1;
  Map.map (increment, m);
};

let fold_op = (m: foobar): foobar => {
  let aggregate = (i: int, j: (int, int)) => i + j[0] + j[1];
  Map.fold (aggregate, m, 10);
};

let deep_op = (m: foobar) : foobar => {
  let coco = (0, m);
  let coco = (0, Map.remove (42, coco[1]));
  let coco = (0, Map.update (32, Some (16), coco[1]));
  coco[1];
};
type storage = int;

type parameter =
| Add (int)
| Sub (int);

type return = (list (operation), storage);

let main = ((action, store): (parameter, storage)) => {
  let store =
    store +
      (switch (action) {
      | Add (n) => n
      | Sub (n) => -n
      });
  (([]: list (operation)), store);
};
type storage = int;

type parameter =
  Increment (int)
| Decrement (int);

let add = ((a: int), (b: int)) => a + b;
let sub = ((a: int), (b: int)) => a - b;

let main = ((action, store) : (parameter, storage)) => {
  let store =
    switch (action) {
    | Increment (n) => add (store, n)
    | Decrement (n) => sub (store, n)
    };
  (([] : list (operation)), store);
};
// Test michelson insertion in ReasonLIGO

let michelson_add = (n : (nat, nat)) : nat =>
  [%Michelson ({| { UNPAIR;ADD } |} : ((nat, nat) => nat)) ](n);
type inner_storage = michelson_pair(int,"one",nat,"two");
type storage = michelson_pair(int,"three",inner_storage,"four");

type return = (list (operation) , storage);

let main = ((action, store) : (unit , storage)) : return => {
  let foo = (3,(1,2n)) ;
  (([] : list(operation)), (foo: storage))
};/* Test function with several parameters */

let abcde_curried =
  (a: int, b: int, c: int, d: int, e: int): int => c + e + 3;

let abcde = (x: (int , int , int , int , int)) : int =>
  abcde_curried (x[0], x[1], x[2], x[3], x[4]);
// storage type

type counter = nat;
type threshold = nat;
type authorized_keys = list (key);
type id = string;

type storage = {
  id        : id,
  counter   : counter,
  threshold : threshold,
  auth      : authorized_keys
};

// I/O types

type message = unit => list (operation);

type dummy = (key_hash,signature);

type signatures = list (dummy); /* Waiting to be fixed */

type check_message_pt = {
  counter    : counter,
  message    : message,
  signatures : signatures
};

type return = (list (operation),storage);

type parameter = CheckMessage (check_message_pt);

let check_message = ((param, s): (check_message_pt, storage)) : return =>
{
  let message : message = param.message;
  let s =
    if (param.counter != s.counter) {
      (failwith ("Counters does not match") : storage);
    } else {
    let packed_payload : bytes =
      Bytes.pack ((message, param.counter, s.id, chain_id));
      let valid : nat = 0n;
      let keys : authorized_keys = s.auth;
      let aux = ((vk, pkh_sig) :
                 ((nat, authorized_keys), (key_hash, signature)))
                 : (nat, authorized_keys) => {
        let (valid, keys) = vk;
        switch (keys) {
        | [] => vk;
        | [key, ...keys] =>
            if (pkh_sig[0] == Crypto.hash_key (key)) {
              let valid =
                if (Crypto.check (key, pkh_sig[1], packed_payload)) {
                  valid + 1n;
                }
                else { (failwith ("Invalid signature") : nat) };
              (valid, keys);
            }
            else { (valid, keys); };
        };
      };
    let (valid, keys) =
      List.fold (aux, param.signatures, (valid, keys));
    if (valid < s.threshold) {
      (failwith ("Not enough signatures passed the check") : storage);
    }
    else {
      {...s,counter : s.counter + 1n};
    };
  };
  (message (unit),s)
};

let main = ((action, store) : (parameter,storage)) : return =>
  switch (action) {
   | CheckMessage (p) => check_message ((p, store))
  }
type f = int

let a = (b: f) => {
  if (b == 2) {
    3
  } else {
    4
  }
}

let c = (c: f) => {
  3
}type foobar = option (int);

let s : foobar = Some (42);
let n : foobar = None;
/* Pledge-Distribute — Accept money from a number of contributors and then donate
   to an address designated by an oracle */

/* A lot of people (myself included) seem to expect an oracle to be more than it is.
   That is, they expect it to be something complicated when it's actually pretty simple.
   An oracle is just an authorized source of information external to the chain, like an
   arbiter or moderator. For example, it's not possible to do an HTTP request to get
   info from a weather site directly using a smart contract. So instead what you
   do is make (or use) an oracle service which uploads the data to the chain so
   that contracts can use it.
*/

type storage = address

type parameter =
  | Donate(unit)
  | Distribute((unit => list(operation)))

let donate = ((p,s): (unit, storage)) : (list(operation), storage) => {
  (([]: list(operation)), s);
};

let distribute = ((p,s): ((unit => list(operation)), storage)) : (list(operation), storage) => {
  if (Tezos.sender == s) {
    (p(),s);
  }
  else {
    (failwith("You're not the oracle for this distribution."):
      (list(operation), storage));
  };
};

let main = ((p,s): (parameter, storage)) : (list(operation), storage) => {
  switch (p) {
    | Donate => donate (((),s))
    | Distribute msg => distribute ((msg,s))
  };
};
type foobar = {
 foo : int,
 bar : int
};

let fb : foobar = {
  foo : 0,
  bar : 0
};

type abc = {
  a : int,
  b : int,
  c : int
};

let abc : abc = {
  a : 42,
  b : 142,
  c : 242
};

let a : int = abc.a;
let b : int = abc.b;
let c : int = abc.c;

let projection = (r: foobar) : int => r.foo + r.bar;

let modify = (r: foobar) : foobar => {foo: 256, bar: r.bar};

let modify_abc = (r: abc) : abc => {...r, b: 2048, c: 42};

type big_record = {
  a : int,
  b : int,
  c : int,
  d : int,
  e : int
};

let br : big_record = {
  a : 23,
  b : 23,
  c : 23,
  d : 23,
  e : 23
};

type double_record = {
  inner : abc,
};

let modify_inner =
  (r: double_record) : double_record => {...r, inner.b : 2048};
// Test while loops in PascaLIGO

let rec sum = ((n, acc) : (int,int)): int =>
    if (n < 1) {acc;} else {sum ((n-1,acc+n));};

let rec fibo = ((n, n_1, n_0) : (int,int,int)): int =>
    if (n < 2) {n_1;} else {fibo ((n-1,n_1+n_0,n_1));};
let main = (p: unit) : address => Tezos.self_address;
/* Test set operations in ReasonLIGO */

let literal_op = (p: unit) : set (string) =>
  Set.literal (["foo", "bar", "foobar"]);

let add_op = (s: set (string)) : set (string) =>
  Set.add ("foobar", s);

let remove_op = (s: set (string)) : set (string) =>
  Set.remove ("foobar", s);

let remove_deep = (s: (set (string), nat)): set (string) =>
  Set.remove ("foobar", s[0]);

let mem_op = (s: set (string)) : bool =>
  Set.mem ("foobar", s);

let size_op = (s: set (string)): nat =>
  Set.cardinal (s);
let main = (p: key_hash) : list (operation) => {
  let unused : operation = (Tezos.set_delegate (Some (p)));
  ([] : list (operation));
} ;
type p = {x: int}

let o = (p: int): p => {
	x: p  
}/* Test that the string concatenation syntax in ReasonLIGO works */

let size_op = (s: string) : nat => String.length (s);
let slice_op = (s: string) : string => String.sub (1n, 2n, s);
let concat_syntax = (s: string) => s ++ "test_literal";
type parameter =
  Increment (int)
| Decrement (int);

type storage = int;

type return = (list (operation), storage);

let main = ((action, store): (parameter, storage)) : return => {
  let store =
    switch (action) {
    | Increment (n) => store + n
    | Decrement (n) => store - n
    };
  ([] : list (operation), store);
};
type abc = (int, int, int);

let projection_abc = (tpl: abc): int => tpl[1];

type foobar = (int, int);

let fb: foobar = (0, 0);

let projection = (tpl: foobar): int => tpl[0] + tpl[1];

type big_tuple = (int, int, int, int, int);

let br: big_tuple = (23, 23, 23, 23, 23);type z = list((int, int));

let o: z = [(2,4), (4, 6)];let sum = ((result, i) : (int, int)) : int => result - i;
let parentheses = (((((result, i)))) : (((int, int)))) : int => result - i;
/*
  The difference between tuples and arguments is subtle in ReasonLIGO.

   `f(a, b);`
   f is called with two arguments

   `f((a, b));`
   f is called with a tuple.

*/

type fun_type = (int, int) => int;

let arguments = (b: int, c: int) => { b + c; };

let arguments_type_def = (b: fun_type) => b (5, 3);

let arguments_test = (_: int) => arguments_type_def (arguments);

type tuple_type = ((int, int)) => int;

let tuple = ((a, b): (int, int)) => { a + b; };

let tuple_type_def = (b: tuple_type) => b ((5, 3));

let tuple_test = (_: int) => tuple_type_def (tuple);


/* inline */

let arguments_inline = (b: int, c: int) => { b + c; };

let arguments_type_def_inline = (b: (int, int) => int) => b (5, 3);

let arguments_test_inline = (_: int) =>
  arguments_type_def_inline (arguments_inline);

let tuple_inline = ((a, b): (int, int)) => { a + b; };

let tuple_type_def_inline = (b: ((int, int)) => int) => b ((5, 3));

let tuple_test_inline = (_: int) =>
  tuple_type_def_inline(tuple_inline);
type storage = (int, string, nat, bool)

type parameter = int

let main = ((p,storage): (parameter, storage)) => {  
([]: list (operation), (2, "2", 2n, false));
};
let a = 1;
let b = 1n;
let c = 2mutez;
let d = 1n + 2n;
let e = 1mutez + 3mutez;
let f = (a, c);
let g = (a + 1, c);
let h = ("a" ++ "2", d);
let i = (a: int, b: int) => a + b;
let j = (a: int, b: int) => a - b;
/* not supported by typer yet: let k = () => b; */
/* not supported by typer yet: let l = () => i(2,3); */
let m = {
    let z = 3;
    z;
};
let n = (a: int): int => a + 1;
let o = (a: int): int => a + 1;
let n = (a: int, b: int): int => a + 1;
let o = (a: int, b: int): int => a + 1;
let p = {{
    3;
}};
let q = {
    f: 3,
    g: 6,
    h: {
        i: "bla",
        j: 1 + 2,
        k: {
            l: 1,
            z: 2
        },
    },
};

/*
Not supported yet by parser:

let r = {
  a: 1
};
*/

let s = {
    let a = 2;
    {
      z: a,
      a
    };
};

let t = (((((((2)))))));
let u = if (true) { 1; } else { 2; };type foobar =
  Foo (int)
| Bar (bool)
| Kee (nat);

let foo: foobar = Foo (42);
let bar: foobar = Bar (true);
let kee: foobar = Kee (23n);
/* IF YOU CHANGE THIS, CHANGE THE EXAMPLE ON THE FRONT PAGE OF THE WEBSITE */

type storage = int;

/* variant defining pseudo multi-entrypoint actions */

type parameter =
| Increment (int)
| Decrement (int);

let add = ((a,b): (int, int)): int => a + b;
let sub = ((a,b): (int, int)): int => a - b;

/* real entrypoint that re-routes the flow based on the parameter provided */

let main = ((p,storage): (parameter, storage)) => {
  let storage =
    switch (p) {
    | Increment(n) => add ((storage, n))
    | Decrement(n) => sub ((storage, n))
    };
  ([]: list (operation), storage);
};

/* IF YOU CHANGE THIS, CHANGE THE EXAMPLE ON THE FRONT PAGE OF THE WEBSITE */
