type tokens = (address, nat) big_map
type allowances = (address * address, nat) big_map (* (sender,account) -> value *)

type storage = {
  tokens      : tokens;
  allowances  : allowances;
  total_amount : nat;
}

type transfer = {
	address_from : address;
	address_to   : address;
	value        : nat;
}

type approve = {
	spender : address;
	value   : nat;
}

type getAllowance = {
	owner    : address;
	spender  : address;
	callback : nat contract;
}

type getBalance = {
	owner    : address;
	callback : nat contract;
}

type getTotalSupply = {
	callback : nat contract;
}

type action =
  	Transfer       of transfer
|	Approve        of approve
|	GetAllowance   of getAllowance
|	GetBalance     of getBalance
|	GetTotalSupply of getTotalSupply

let transfer (p,s : transfer * storage) : operation list * storage =
   let new_allowances =   
		if Tezos.sender = p.address_from then s.allowances
		else
			let authorized_value = match Big_map.find_opt (Tezos.sender,p.address_from) s.allowances with
				Some value -> value
			|	None       -> 0n
			in
			if (authorized_value < p.value)
			then (failwith "Not Enough Allowance" : allowances)
			else Big_map.update (Tezos.sender,p.address_from) (Some (abs(authorized_value - p.value))) s.allowances
   in    
	let sender_balance = match Big_map.find_opt p.address_from s.tokens with
		Some value -> value
	|	None        -> 0n
	in
	if (sender_balance < p.value)
	then (failwith "Not Enough Balance" : operation list * storage)
	else
		let new_tokens = Big_map.update p.address_from (Some (abs(sender_balance - p.value))) s.tokens in
		let receiver_balance = match Big_map.find_opt p.address_to s.tokens with
			Some value -> value
		|	None        -> 0n
		in
		let new_tokens = Big_map.update p.address_to (Some (receiver_balance + p.value)) new_tokens in
		([]:operation list), {s with tokens = new_tokens; allowances = new_allowances}

let approve (p,s : approve * storage) : operation list * storage =
	let previous_value = match Big_map.find_opt (p.spender, Tezos.sender) s.allowances with
		Some value -> value
	|	None -> 0n
	in
	if previous_value > 0n && p.value > 0n
	then (failwith "Unsafe Allowance Change" : operation list * storage)
	else
		let new_allowances = Big_map.update (p.spender, Tezos.sender) (Some (p.value)) s.allowances in
		([] : operation list), {s with allowances = new_allowances}

let getAllowance (p,s : getAllowance * storage) : operation list * storage =
	let value = match Big_map.find_opt (p.owner, p.spender) s.allowances with
		Some value -> value
	|	None -> 0n
	in
	let op = Tezos.transaction value 0mutez p.callback in
	([op],s)

let getBalance (p,s : getBalance * storage) : operation list * storage =
	let value = match Big_map.find_opt p.owner s.tokens with
		Some value -> value
	|	None -> 0n
	in
	let op = Tezos.transaction value 0mutez p.callback in
	([op],s)

let getTotalSupply (p,s : getTotalSupply * storage) : operation list * storage =
  let total = s.total_amount in
  let op    = Tezos.transaction total 0mutez p.callback in
  ([op],s)


let main (a,s:action * storage) = 
 	match a with
   	Transfer p -> transfer (p,s)
	|	Approve  p -> approve (p,s)
	|	GetAllowance p -> getAllowance (p,s)
	|  GetBalance p -> getBalance (p,s)
	|	GetTotalSupply p -> getTotalSupply (p,s)
let main (p : key_hash) =
  let c : unit contract = Tezos.implicit_account p
  in Tezos.address c
let check_ (p : unit) : int = if Tezos.amount = 100tez then 42 else 0
(* should return a constant function *)
let f1 (x : unit) : unit -> tez =
  let amt : tez = Current.amount in
  fun (x : unit) -> amt

(* should return an impure function *)
let f2 (x : unit) : unit -> tez =
  fun (x : unit) -> Current.amount

let main (b,s : bool * (unit -> tez)) : operation list * (unit -> tez) =
  (([] : operation list), (if b then f1 () else f2 ()))
type comb_two = [@layout:comb] {
  [@annot:anbfoo]
  foo : int ;
  [@annot:anabar]
  bar : string ;
}

type comb_three = [@layout:comb] {
  [@annot:ana]
  a : int ;
  [@annot:anb]
  b : string ;
  [@annot:anc] 
  c : nat ;
}

type comb_five = [@layout:comb] {
  [@annot:an_One]
  one : int ;
  [@annot:an_Two]
  two : string ;
  [@annot:an_Three]
  three : bool;
  [@annot:an_Four]
  four : nat ;
  [@annot:an_Five]
  five : int ;
}

type parameter = unit
type op_list = operation list

let main_comb_two (action, store : parameter * comb_two ) : op_list * comb_two =
  let o = store.foo in
  let oo = { store with foo = o } in
  ([] : operation list), oo

let main_comb_three (action, store : parameter * comb_three ) : op_list * comb_three =
  ([] : operation list), { a = 1 ; b = "" ; c = 1n }

let main_comb_five (action, store : parameter * comb_five ) : op_list * comb_five =
  ([] : operation list), store

let r : comb_five = { one = 1 ; two = "" ; three = true ; four = 1n ; five = 2 }
let accesses = r.one , r.two , r.three , r.four , r.five
type comb_two = [@layout:tree] {
  [@annot:anfoo]
  foo : int ;
  [@annot:anbar]
  bar : string ;
}

type comb_three = [@layout:tree] {
  [@annot:ana]
  a : int ;
  [@annot:anb]
  b : string ;
  [@annot:anc] 
  c : nat ;
}

type comb_five = [@layout:tree] {
  [@annot:an_One]
  one : int ;
  [@annot:an_Two]
  two : string ;
  [@annot:an_Three]
  three : bool;
  [@annot:an_Four]
  four : nat ;
  [@annot:an_Five]
  five : int ;
}

type parameter = unit
type op_list = operation list

let main_comb_two (action, store : parameter * comb_two ) : op_list * comb_two =
  let o = store.foo in
  let oo = { store with foo = o } in
  ([] : operation list), oo

let main_comb_three (action, store : parameter * comb_three ) : op_list * comb_three =
  ([] : operation list), { a = 1 ; b = "" ; c = 1n }

let main_comb_five (action, store : parameter * comb_five ) : op_list * comb_five =
  ([] : operation list), store
  type comb_two = [@layout:comb]
  | [@annot:anbfoo] Foo of int
  | [@annot:anabar] Bar of string

type comb_three = [@layout:comb]
  | [@annot:ana] A of int
  | [@annot:anb] B of string
  | [@annot:anc] C of nat

type comb_five = [@layout:comb]
  | [@annot:an_One] One of int
  | [@annot:an_Two] Two of string
  | [@annot:an_Three] Three of bool
  | [@annot:an_Four] Four of nat
  | [@annot:an_Five] Five of int

type parameter = unit
type op_list = operation list


let main_comb_two (action, store : parameter * comb_two ) : op_list * comb_two =
  let o = match store with
    | Foo i -> Bar "foo"
    | Bar j -> Foo 1
  in
 ([] : operation list), o
 
let main_comb_three (action, store : parameter * comb_three ) : op_list * comb_three =
  let o = (C 1n) in
  ([] : operation list), o
  
let main_comb_five (action, store : parameter * comb_five ) : op_list * comb_five =
  let o = match store with
    | One a -> Five (1)
    | Two a -> Four (2n)
    | Three a -> Three (true)
    | Four a -> Two ("lol")
    | Five a -> One 1
  in
  ([] : operation list), o
  type comb_two = [@layout:tree]
  | [@annot:anbfoo] Foo of int
  | [@annot:anabar] Bar of string

type comb_three = [@layout:tree]
  | [@annot:ana] A of int
  | [@annot:anb] B of string
  | [@annot:anc] C of nat

type comb_five = [@layout:tree]
  | [@annot:an_One] One of int
  | [@annot:an_Two] Two of string
  | [@annot:an_Three] Three of bool
  | [@annot:an_Four] Four of nat
  | [@annot:an_Five] Five of int

type parameter = unit
type op_list = operation list


let main_comb_two (action, store : parameter * comb_two ) : op_list * comb_two =
  let o = match store with
    | Foo i -> Bar "foo"
    | Bar j -> Foo 1
  in
 ([] : operation list), o
 
let main_comb_three (action, store : parameter * comb_three ) : op_list * comb_three =
  let o = (C 1n) in
  ([] : operation list), o
  
let main_comb_five (action, store : parameter * comb_five ) : op_list * comb_five =
  let o = match store with
    | One a -> Five (1)
    | Two a -> Four (2n)
    | Three a -> Three (true)
    | Four a -> Two ("lol")
    | Five a -> One 1
  in
  ([] : operation list), o
let mod_op   (n : int) : nat = n mod 42
let plus_op  (n : int) : int = n + 42
let minus_op (n : int) : int = n - 42
let times_op (n : int) : int = n * 42
let div_op   (n : int) : int = n / 2
let neg_op   (n : int) : int = -n
let foo      (n : int) : int = n + 10
let neg_op_2 (b : int) : int = -(foo b)
let ediv_op  (n : int) : (int * nat) option = ediv n 2
let main (p, s : bool * unit) =
  let u : unit = assert p
  in ([] : operation list), s

let some (o : unit option) =
  assert_some o
[@inline] let x = 1
[@inline] let foo (a : int): int =
  ([@inline] let test = 2 + a in test)
[@inline][@other] let y = 1
let bar (b : int): int =
  [@inline][@foo][@bar] let test = fun (z : int) -> 2 + b + z in
  test b
(*
This test makes sure that the balance is accessible in CameLIGO.

It is there to detect a regression of:
https://gitlab.com/ligolang/ligo/issues/61

which results in this error when you attempt to compile this contract:

generated. unrecognized constant: {"constant":"BALANCE","location":"generated"}

*)

type parameter = unit
type storage = tez
type return = operation list * storage

let main (p, s : parameter * storage) : return =
  ([] : operation list), Tezos.balance
type toto = int

let foo : toto = 42 + 127
type foo = (int, int) big_map

let set_ (n, m: int * foo) : foo = Big_map.update 23 (Some n) m
let add (n, m : int * foo) : foo = Big_map.add 23 n m

let rm (m : foo) : foo = Big_map.remove 42 m
let gf (m : foo) : int = Big_map.find 23 m

let get (m : foo): int option = Big_map.find_opt 42 m

let empty_map : foo = Big_map.empty

let map1 : foo = Big_map.literal [(23,0); (42,0)]
let map1 : foo = Big_map.literal [(23,0); (42,0)]

let mutimaps (m : foo) (n : foo) : foo =
  let bar : foo = Big_map.update 42 (Some 0) m
  in Big_map.update 42 (get bar) n
(* Test CameLIGO bitwise operators *)

let or_op  (n : nat) : nat = Bitwise.or  n 4n
let and_op (n : nat) : nat = Bitwise.and n 7n
let xor_op (n : nat) : nat = Bitwise.xor n 7n
let lsl_op (n : nat) : nat = Bitwise.shift_left n 7n
let lsr_op (n : nat) : nat = Bitwise.shift_right n 7n


let or_op_infix  (n : nat) : nat = n lor 4n
let and_op_infix (n : nat) : nat = n land 7n
let xor_op_infix (n : nat) : nat = n lxor 7n
let lsl_op_infix (n : nat) : nat = n lsl 7n
let lsr_op_infix (n : nat) : nat = n lsr 7n
// Test CameLIGO boolean operators

let or_true   (b : bool) : bool = b || true
let or_false  (b : bool) : bool = b || false
let and_true  (b : bool) : bool = b && true
let and_false (b : bool) : bool = b && false
let not_bool  (b : bool) : bool = not b
let concat_op (s : bytes) : bytes = Bytes.concat s 0x7070
let slice_op  (s : bytes) : bytes = Bytes.sub 1n 2n s
let hasherman (s : bytes) : bytes = Crypto.sha256 s
let id_string (p : string) : string option =
  let packed : bytes = Bytes.pack p in
  (Bytes.unpack packed : string option)

let id_int (p : int) : int option =
  let packed : bytes = Bytes.pack p in
  (Bytes.unpack packed : int option)

let id_address (p : address) : address option =
  let packed : bytes = Bytes.pack p in
  (Bytes.unpack packed : address option)
let check_signature (pk, signed, msg : key * signature * bytes) : bool =
  Crypto.check pk signed msg

(*
$ tezos-client gen keys testsign

$ tezos-client show address testsign -S
Hash: tz1RffmtWjy435AXZuWwLWG6UaJ66ERmgviA
Public Key: edpktz4xg6csJnJ5vcmMb2H37sWXyBDcoAp3XrBvjRaTSQ1zmZTeRQ
Secret Key: unencrypted:edsk34mH9qhMdVWtbammJfYkUoQfwW6Rw5K6rbGW1ajppy3LPNbiJA

$ tezos-client hash data '"hello"' of type string
Raw packed data: 0x05010000000568656c6c6f
...

$ tezos-client sign bytes 0x05010000000568656c6c6f for testsign
Signature: edsigtnzKd51CDomKVMFBoU8SzFZgNqRkYUaQH4DLUg8Lsimz98DFB82uiHAkdvx29DDqHxPf1noQ8noWpKMZoxTCsfprrbs4Xo
*)

let example : bool =
  Crypto.check
    ("edpktz4xg6csJnJ5vcmMb2H37sWXyBDcoAp3XrBvjRaTSQ1zmZTeRQ" : key)
    ("edsigtnzKd51CDomKVMFBoU8SzFZgNqRkYUaQH4DLUg8Lsimz98DFB82uiHAkdvx29DDqHxPf1noQ8noWpKMZoxTCsfprrbs4Xo" : signature)
    0x05010000000568656c6c6f
(* Test whether closures capture variables in CameLIGO *)

let test (k : int) : int =
  let j : int = k + 5 in
  let close : int -> int = fun (i : int) -> i + j in
  let j : int = 20 (* Shadow original variable *)
  in close 20
(* This test check that the type are comparable *)

let int_ (a: int) = a < a
let nat_ (a: nat) = a < a
let bool_ (a: bool) = a < a
let mutez_ (a: tez) = a < a
let string_ (a: string) = a < a
let bytes_ (a: bytes) = a < a
let address_ (a: address) = a < a
let timestamp_ (a: timestamp) = a < a
let key_hash_ (a: key_hash) = a < a

type comp_pair = int * int

let comp_pair (a: comp_pair) = a < a

(* 
type uncomp_pair_1 = int * int * int

let uncomp_pair_1 (a: uncomp_pair_1) = a < a

type uncomp_pair_2 = comp_pair * int

let uncomp_pair_2 (a: uncomp_pair_2) = a < a
*)

type inner_record = (int,"one",nat,"two") michelson_pair
type comb_record = (int,"three",inner_record,"four") michelson_pair

let comb_record (a : comb_record) = a < a
type integer = int

let main (i : int) =
  if (i = 2 : bool) then (42 : int) else (0 : integer)
let main (i : int) =
  let result = 0 in
  if i = 2
  then let result = 42 in result
  else let result = 0 in result
// Test conditional in CameLIGO

let main (i : int) = if i = 2 then 42 else 0
type storage = int

let main (p, s : int * storage) = ([] : operation list), p + s
type return = operation list * string

let main (action, store : string * string) : return =
  let toto : operation * address = Tezos.create_contract
    (fun (p, s : nat * string) -> (([] : operation list), "one")) 
    (None: key_hash option) 
    300tz 
    "un"
  in
  ([toto.0], store)let hasherman512 (s : bytes) : bytes = Crypto.sha512 s
let hasherman_blake (s : bytes) : bytes = Crypto.blake2b s
let conv_test (j : int) (k : int) = j + k
let main (i : int) : int = conv_test i 10
let partial (a : int) (b : int) : int = a + b
let mk_partial (j : int) : int -> int = partial j
let partial_apply (i : int) : int = mk_partial 10 i
type storage = (int,"foo",string,"bar") michelson_or 
type foobar = (int,"baz", int, "fooo" ) michelson_or

type return = operation list * storage

let main (action, store : unit * storage) : return =
  let foo = (M_right ("one") : storage) in
  let bar = (M_right 1 : foobar) in
  (([] : operation list), (foo: storage))
type foo =
  Bar of int
| Baz

let main (f : foo) : int =
  match f with
    Bar i -> i
  | Baz -> -1
// Test conditional in CameLIGO

let main (a, b : bool * bool) = if a = b then 999 else 1

type storage = unit

let main (p, store : unit * storage) : operation list * storage =
  (failwith "This contract always fails" : operation list * storage)
type storage = unit

let main (p, store : unit * storage) : operation list * storage =
  let n =
    (fun (f : int * int -> int) (x : int) (y : int) -> f (y,x))
      (fun (x : int) (y : int) -> x + y)
      0
      1
  in ([] : operation list), store
type storage = unit

let main (p, store : unit * storage) : operation list * storage =
  let n =
    (fun (f : int -> int) (z : int) (y : int) -> f y)
      (fun (x : int) -> x)
      0
      1
  in ([] : operation list), store
type storage = unit

let main (p, s : unit * storage) : operation list * storage =
  let n =
    (fun (f : int -> int -> int) (x : int) (y : int) -> f y (x+y))
      (fun (x : int) (y : int) -> x + y)
      0
      1
  in ([] : operation list), store
type storage = unit

let main (p, s : unit * storage) =
  (fun (f : int -> int) (x : int) -> f x)
    (fun (x : int) -> x)
    1
(* Test use of multiple subroutines in a CameLIGO function *)

let foo (i : int) : int = i + 20
let bar (i : int) : int = i + 50
let foobar (i : int) : int = foo i + bar i
type storage = {
  challenge : string
}

type param = {
  new_challenge : string;
  attempt       : string
}

type return = operation list * storage

let attempt (p, store : param * storage) : return =
  (* if p.attempt <> store.challenge then failwith "Failed challenge" else *)
  let contract : unit contract =
    match (Tezos.get_contract_opt Tezos.sender : unit contract option) with
      Some contract -> contract
    | None ->  (failwith "No contract" : unit contract)
  in
  let transfer : operation =
    Tezos.transaction (unit, contract, 10.00tez) in
  let store : storage = {challenge = p.new_challenge}
  in ([] : operation list), store
type commit = {
  date        : timestamp;
  salted_hash : bytes;
}

type commit_set = (address, commit) big_map

type storage = {
  hashed  : bytes;
  unused  : bool;
  commits : commit_set
}

type reveal = {
  hashable : bytes;
  message  : unit -> operation list
}

type parameter =
  Commit of bytes
| Reveal of reveal

type return = operation list * storage

(* We use hash-commit so that a baker can not steal *)

let commit (p, s : bytes * storage) : return =
  let commit : commit =
    {date = Tezos.now + 86_400; salted_hash = p} in
  let updated_map: commit_set =
    Big_map.update Tezos.sender (Some commit) s.commits in
  let s = {s with commits = updated_map}
  in ([] : operation list), s

let reveal (p, s : reveal * storage) : return =
  if not s.unused
  then
    (failwith "This contract has already been used." : return)
  else
    let commit : commit =
      match Big_map.find_opt sender s.commits with
    | Some c -> c
    | None ->
       (failwith "You have not made a commitment to hash against yet."
        : commit)
    in
    if Tezos.now < commit.date
    then
      (failwith "It has not been 24 hours since your commit yet.": return)
    else
      let salted =
        Crypto.sha256 (Bytes.concat p.hashable (Bytes.pack sender)) in
      if salted <> commit.salted_hash
      then
        (failwith "This reveal does not match your commitment.": return)
      else
        if s.hashed = Crypto.sha256 p.hashable
        then
          let s : storage = {s with unused = false}
          in p.message (), s
        else (failwith "Your commitment did not match the storage hash."
              : return)

let main (p, s : parameter * storage) : return =
  match p with
  | Commit c -> commit (c,s)
  | Reveal r -> reveal (r,s)
(* Test a function which takes another function as an argument *)

let foobar (i : int) : int =
  let foo: (int -> int) = fun (i : int) -> i in
  let bar: ((int -> int) -> int) = fun (f : int -> int) -> f i
  in bar foo

(* higher order function with more than one argument *)

let higher2 (i : int) (f : int -> int): int =
  let ii: int = f i in ii

let foobar2 (i : int) : int =
  let foo2 : (int -> int) = fun (i : int) -> i
  in higher2 i foo2

let a : int = 0

let foobar3 (i : int) : int =
  let foo2 : (int -> int) = fun (i : int) ->  a + i
  in higher2 i foo2

let f (i : int) : int = i

let g (i : int) : int = f i

let foobar4 (i : int) : int = g (g i)

let higher3 (i : int) (f : int -> int) (g : int -> int) : int =
  let ii : int = f (g i) in ii

let foobar5 (i : int) : int =
  let a : int = 0 in
  let foo : (int -> int) = fun (i : int) -> a + i in
  let goo : (int -> int) = fun (i : int) -> foo i
  in higher3 i foo goo
type id = int

type id_details = {
  owner: address;
  controller: address;
  profile: bytes
}

type buy = {
  profile: bytes;
  initial_controller: address option;
}

type update_owner = {
  id: id;
  new_owner: address;
}

type update_details = {
  id: id;
  new_profile: bytes option;
  new_controller: address option;
}

type action =
| Buy of buy
| Update_owner of update_owner
| Update_details of update_details
| Skip

(* The prices kept in storage can be changed by bakers, though they
   should only be adjusted down over time, not up. *)

(* The prices kept in storage can be changed by bakers, though they should only be
   adjusted down over time, not up. *)
type storage = {
  identities: (id, id_details) big_map;
  next_id: int;
  name_price: tez;
  skip_price: tez;
}

type return = operation list * storage

(* Preliminary thoughts on ids:

I very much like the simplicity of http://gurno.com/adam/mne/.  5 three
letter words means you have a 15 character identity, not actually more
annoying than an IP address and a lot more memorable than the raw
digits. This can be stored as a single integer which is then
translated into the corresponding series of 5 words.

I in general like the idea of having a 'skip' mechanism, but it does
need to cost something so people don't eat up the address space. 256^5
means you have a lot of address space, but if people troll by skipping
a lot that could be eaten up.  Should probably do some napkin
calculations for how expensive skipping needs to be to deter people
from doing it just to chew up address space.  *)

let buy (parameter, storage: buy * storage) =
  let void: unit = 
    if amount = storage.name_price 
    then () 
    else (failwith "Incorrect amount paid.": unit)
  in
  let profile = parameter.profile in
  let initial_controller = parameter.initial_controller in
  let identities = storage.identities in
  let new_id = storage.next_id in
  let controller: address =
    match initial_controller with
    | Some addr -> addr
    | None -> sender in
  let new_id_details: id_details = {
    owner = sender;
    controller = controller;
    profile = profile} in
  let updated_identities : (id, id_details) big_map =
    Big_map.update new_id (Some new_id_details) identities
  in
  ([]: operation list), {storage with identities = updated_identities;
                         next_id = new_id + 1; 
                        }

let update_owner (parameter, storage: update_owner * storage) =
  if (amount <> 0mutez)
  then (failwith "Updating owner doesn't cost anything.": (operation list) * storage)
  else
  let id = parameter.id in
  let new_owner = parameter.new_owner in
  let identities = storage.identities in
  let current_id_details: id_details =
    match Big_map.find_opt id identities with
    | Some id_details -> id_details
    | None -> (failwith "This ID does not exist.": id_details)
  in
  let u : unit =
    if sender = current_id_details.owner
    then ()
    else failwith "You are not the owner of this ID."
  in
  let updated_id_details: id_details = {
    owner = new_owner;
    controller = current_id_details.controller;
    profile = current_id_details.profile;
  }
  in
  let updated_identities = Big_map.update id (Some updated_id_details) identities in
  ([]: operation list), {storage with identities = updated_identities}

let update_details (parameter, storage: update_details * storage) =
  if (amount <> 0mutez)
  then (failwith "Updating details doesn't cost anything.": (operation list) * storage)
  else
  let id = parameter.id in
  let new_profile = parameter.new_profile in
  let new_controller = parameter.new_controller in
  let identities = storage.identities in
  let current_id_details: id_details =
    match Big_map.find_opt id identities with
    | Some id_details -> id_details
    | None -> (failwith "This ID does not exist.": id_details)
  in
  let u : unit =
    if (sender = current_id_details.controller) || (sender = current_id_details.owner)
    then ()
    else failwith ("You are not the owner or controller of this ID.")
  in
  let owner: address = current_id_details.owner in
  let profile: bytes =
    match new_profile with
    | None -> (* Default *) current_id_details.profile
    | Some new_profile -> new_profile
  in
  let controller: address =
    match new_controller with
    | None -> (* Default *) current_id_details.controller
    | Some new_controller -> new_controller
  in
  let updated_id_details: id_details = {
    owner = owner;
    controller = controller;
    profile = profile;
  }
  in
  let updated_identities: (id, id_details) big_map  =
    Big_map.update id (Some updated_id_details) identities in
  ([]: operation list), {storage with identities = updated_identities}

(* Let someone skip the next identity so nobody has to take one that's undesirable *)
let skip (p,storage: unit * storage) =
  let void: unit =
    if amount = storage.skip_price
    then ()
    else failwith "Incorrect amount paid."
  in
  ([]: operation list), {storage with next_id = storage.next_id + 1}

let main (action, storage : action * storage) : return =
  match action with
  | Buy b -> buy (b, storage)
  | Update_owner uo -> update_owner (uo, storage)
  | Update_details ud -> update_details (ud, storage)
  | Skip -> skip ((), storage)
let main2 (p : key_hash) (s : unit) =
  let c : unit contract = Tezos.implicit_account p
  in ([] : operation list), unit

let main (p,s : key_hash * unit) = main2 p s
let main (kh : key_hash) : unit contract = Tezos.implicit_account kh
// Demonstrate CameLIGO inclusion statements, see includer.mligo

let foo : int = 144
// Demonstrate CameLIGO inclusion statements, see included.mligo

let bar : int = foo
type storage = int

type parameter =
  Increment of int
| Decrement of int

type return = operation list * storage

let add (a : int) (b : int) : int = a + b
let sub (a : int) (b : int) : int = a - b

let main (action, store : parameter * storage) : return =
  let store =
    match action with
      Increment n -> add s n
    | Decrement n -> sub s n
  in ([] : operation list), store
let lambda_call =
  let a = 3 in
  let foo = fun (i : int) -> i * i in
  (foo (a + 1)) = 16

let higher_order1 =
  let a = 2 in
  let foo = fun (i : int) (j : int) (k : int) -> a + i + j + 0 in
  let bar = (foo 1 2) in
  (bar 3 = 5)

let higher_order2 =
  let a = 2 in
  let foo = fun (i : int) ->
              let b = 2 in
              let bar = fun (i : int) -> i + a + b
              in bar i
  in
  (foo 1 = 5)

let higher_order3 =
  let foo = fun (i : int) -> i + 1 in
  let bar = fun (f : int -> int) (i : int) -> f i + 1 in
  let baz : int -> int = bar foo
  in
  (baz 3 = 5)

let higher_order4 =
  let a = 3 in
  let foo = fun (i : int) -> a + i in
  let bar : int -> int = fun (i : int) -> foo i
  in
  (bar 2 = 5)

let concats = (0x70 ^ 0x70  = 0x7070)

type foo_record = {
  a : string;
  b : string
}

let record_concat =
  let ab : foo_record = {a="a"; b="b"}
  in (ab.a ^ ab.b = "ab")

let record_patch =
  let ab : foo_record = {a="a"; b="b"} in
  let res = {ab with b = "c"} in 
  (res.b = "c")

type bar_record = {
  f   : int -> int;
  arg : int
}

let record_lambda =
  let a = 1 in
  let foo : int -> int = fun (i : int) -> a + i*2 in
  let farg : bar_record = {f = foo; arg = 2}
  in (farg.f farg.arg = 5)

type foo_variant =
| Foo
| Bar of int
| Baz of string

let variant_match =
  let a = Bar 1 in
  match a with
  | Foo   -> false
  | Bar i -> true
  | Baz s -> false

(* UNSUPPORTED: No deep patterns yet.
type bar_variant =
| Baz
| Buz of int * int
| Biz of int * int * string

let long_variant_match =
  let a = Biz (1,2,"Biz") in
  match a with
  | Baz -> "Baz"
  | Buz (a,b) -> "Buz"
  | Biz (a,b,c) -> c
 *)

let bool_match =
  let b = true in
  match b with
  | True -> True
  | True -> False

let list_match =
  let a = [1; 2; 3; 4] in
  match a with
  | hd::tl -> true
  | [] -> false

let tuple_proj =
  let a, b = true, false
  in a or b

let list_const =
  let a = [1; 2; 3; 4] in
  (List.length (0::a)) = 5n

type foobar = int option

let options_match_some =
  let a = Some 0 in
  match a with
  | Some i -> true
  | None -> false

let options_match_none =
  let a : foobar = None in
  match a with
  | Some i -> false
  | None -> true

let is_nat_yes =
  let i : int = 1 in
  match (is_nat i) with
  | Some i -> true
  | None -> false
  
let is_nat_no =
  let j : int = -1 in
  match (is_nat j) with
  | Some i -> false
  | None -> true

let abs_int =
  let a : nat = abs (-5) in
  a = 5n

let nat_int = ((int 5n) = 5)

let map_list =
  let a = [1; 1; 1; 1] in
  let add_one : (int -> int) = fun (i : int) -> i + 1 in
  match (List.map add_one a) with
  | hd::tl -> (hd = 2)
  | [] -> false
  
let fold_list =
  let a = [1; 2; 3; 4] in
  let acc : int * int -> int =
    fun (prev, el : int * int) -> prev + el
  in
  (List.fold acc a 0) = 10

let comparison_int =
  let a = 1 > 2 in
  let b = 2 > 1 in
  let c = 1 >= 2 in
  let d = 2 >= 1 in
  ( not(a) && b && (not c) && d )

let comparison_string = not("foo"="bar") && ("baz"="baz")

let divs_int =
  let a = 1/2 in
  (* let b = 1/2n in *)
  (* let c = 1n/2 in *)
  (a = 0)

let divs_nat =
  let a = 1n/2n in
  let b = 1tz/2tz in
  (a = 0n) && (a = b)

let divs_tez =
  let a = 1tz/2n in
  (a = 0.5tz)

let var_neg =
  let a = 2 in
  (-a = -2)

let sizes =
  let a = [1; 2; 3; 4; 5] in
  let b = "12345" in
  let c = Set.literal [1; 2; 3; 4; 5] in
  let d = Map.literal [(1,1); (2,2); (3,3) ; (4,4) ; (5,5)] in
  let e = 0xFFFF in
  (List.length a = 5n) &&
  (String.length b = 5n) &&
  (Set.cardinal c = 5n) &&
  (Map.size d = 5n) &&
  (Bytes.length e = 2n)

let modi = (3 mod 2 = 1n)

let fold_while =
  let aux : int -> bool * int =
    fun (i : int) ->
    if i < 10 then Loop.resume (i + 1) else Loop.stop i
  in
  (Loop.fold_while aux 20 = 20) &&  (Loop.fold_while aux 0 = 10)

let assertion_pass =
  let unitt = assert (1=1) in
  true


let main (i : int) : nat option = is_nat i

let check_hash_key (kh1, k2 : key_hash * key) : bool * key_hash =
  let kh2 : key_hash = Crypto.hash_key k2
  in kh1 = kh2, kh2
type storage = unit

(* not supported yet
let main (p, s : unit * storage) = (fun x -> ()) ()
*)

let main (p, s : unit * storage) = (fun (_ : unit) -> ()) ()
type storage = unit

(* Not supported yet:
let main (a, s : unit * storage) = (fun x -> ()) ()
*)

let main (a, s : unit * storage) =
  (fun (f : unit -> unit) -> f ()) (fun (_ : unit) -> unit)
type foo = { a :int ; b : nat }
type bar = { c : int * nat ; d : foo }

let t1 =
  let (a,b,c,d) = (1,2,3,4) in
  a

let t2 : string =
  let (a,(b,c),(d,(e,f))) = (1,(2,3),(4,(5,"7"))) in
  f

let t3 =
  let (a,(b,(c,(d,e,f)))) = (1,(2,(3n,(4,5,"7")))) in
  (a+b,c,f)

let t4 =
  let (a,(b,(c,(d,(e,f))),g)) = (1,(1n,(1,(1n,(1,1n))),1)) in
  (a+c+e+g , b + d + f)

let t5 : nat =
  let { a = a ; b = b } = { a = 1 ; b = 1n } in
  b

let t6 =
  let x : foo =  { a = 1 ; b = 2n } in
  let ({ a = a ; b = b },(c,d)) = (x,(1,1)) in
  (a + c + d, b)

let t7 =
  let ( { c = (a,b) ; d = { a = c ; b = d } } ) = { c = ( 1 , 2n) ; d = { a = 1 ; b = 1n } } in
  ( a + c , b + d )

let t8 =
  let (x, (y, { a = a ; b = b })) = (1, (1n, {a = 1 ; b = 1n})) in
  (x + a , y + b)let sum (p : int * int) : int =
  let i, result = p in i + result

let sum2 (p: string * string * string * string) : string =
  let a, b, c, d = p in a ^ b ^ c ^ d
(* Simple test of binding multiple values *)

let (x : int), (y : int) = 1,2

let main (p : unit) : int = x + y

let ((x : int) , (y : int)) = 3,3

let main_paren (p : unit) : int = x + y

let foobar : (int * int) = (23 , 42)
let (foo : int) , (bar : int) = foobar

(* Here to prevent a regression of https://gitlab.com/ligolang/ligo/issues/63#note_254106580 *)

let correct_values_bound (p : unit) : int * int = foo, bar

let non_tuple_rhs (p : unit) : int = bar - foo

(* Here to prevent a regression of https://gitlab.com/ligolang/ligo/issues/63#note_254106580 *)

let big_tuple : int * int * int * int * int = 10, 20, 30, 40, 50

let (a: int), (b: int), (c: int), (d: int), (e: int) = big_tuple

let correct_values_big_tuple (p : unit) : int * int * int * int * int =
  a, b, c, d, e

(* Here to prevent a regression of https://gitlab.com/ligolang/ligo/issues/63#note_254106580 *)

let different_types : int * string = 10, "hello"

let (greet_num : int), (greeting : string) = different_types

let correct_values_different_types (p : unit) : int * string =
  greet_num, greeting
type storage = int * int

let main (n : int * storage) : operation list * storage =
  let x : int * int =
    let x : int = 7
    in x + n.0, n.1.0 + n.1.1
  in ([] : operation list), x


let f0 (a: string) = true
let f1 (a: string) = true
let f2 (a: string) = true

let letin_nesting (_: unit) =
  begin
    let s = "test" in
    let p0 = f0 s in
    assert p0;
    let p1 = f1 s in
    assert p1;
    let p2 = f2 s in
    assert p2;
    s
  end

let letin_nesting2 (x: int) = 
  let y = 2 in 
  let z = 3 in
  x + y + z

let x =
  let (_, (x, _)) = (1n, (2n, 3n)) in 
  x
type storage = int * int list

type parameter = int list

type return = operation list * storage

let x : int list = []
let y : int list = [3; 4; 5]
let z : int list = 2::y

let main (p, s: parameter * storage) : return =
  let storage =
    match p with
          [] -> s
    | hd::tl -> s.0 + hd, tl
  in ([] : operation list), storage

let size_ (s : int list) : nat = List.length s

let fold_op (s : int list) : int =
  let aggregate = fun (t : int * int) -> t.0 + t.1
  in List.fold aggregate s 10

let map_op (s : int list) : int list =
  List.map (fun (cur : int) -> cur + 1) s

let iter_op (s : int list) : unit =
  let do_nothing = fun (_ : int) -> unit
  in List.iter do_nothing s
let local_type ( u : unit) : int =
	type toto = int in
	let titi : toto = 1 in
	let titi = titi + 2 in
	titi
(* Test functional iterators in CameLIGO *)

let rec aux_simple (i : int) : int =
  if i < 100 then aux_simple (i + 1) else i

let counter_simple (n : int) : int = aux_simple n

type sum_aggregator = {
  counter : int;
  sum : int
}

let counter (n : int) : int =
  let initial : sum_aggregator = {counter=0; sum=0} in
  let rec aggregate : sum_aggregator -> int = fun (prev: sum_aggregator) ->
    if prev.counter <= n then
      aggregate {counter = prev.counter + 1;
                   sum = prev.counter + prev.sum}
    else
      prev.sum 
  in
  aggregate initial

let rec aux_nest (prev : sum_aggregator) : int =
  if prev.counter < 100 then
    let sum = prev.sum + (aux_simple prev.counter) in
    aux_nest {counter = prev.counter + 1; sum = sum}
  else
    prev.sum

let counter_nest (n : int) : int =
  let initial : sum_aggregator = {counter=0; sum=0} in
  let out = aux_nest initial
  in out
type foobar = (int, int) map

let empty_map : foobar = Map.empty

let map1 : foobar =
  Map.literal [(144,23); (51,23); (42,23); (120,23); (421,23)]

let map2 : foobar = Map.literal [(23,0); (42,0)]

let set_2 (n : int) (m : foobar) : foobar = Map.update 23 (Some n) m

let set_ (t : int * foobar) : foobar = set_2 t.0 t.1

let add (n,m : int * foobar) : foobar = Map.add 23 n m

let rm (m : foobar) : foobar = Map.remove 42 m

(* Dummy test so that we can add the same test for PascaLIGO *)

let patch_ (m : foobar) : foobar = Map.literal [(0,5); (1,6); (2,7)]

(* Second dummy test, see above *)

let patch_empty (m : foobar) : foobar = Map.literal [(0,0); (1,1); (2,2)]

(* Third dummy test, see above *)

let patch_deep (m : foobar * nat) : foobar * nat =
  Map.literal [(0,0); (1,9); (2,2)], 10n

let size_ (m : foobar) : nat = Map.size m

let get  (m : foobar) : int option = Map.find_opt 42 m
let get_ (m : foobar) : int option = Map.find_opt 42 m

let mem (k,m : int * foobar) : bool = Map.mem k m

let iter_op (m : foobar) : unit =
  let assert_eq = fun (a, b : int * int) -> assert (a = b)
  in Map.iter assert_eq m

let map_op (m : foobar) : foobar =
  let increment = fun (i : int * int) -> i.1 + 1
  in Map.map increment m

let fold_op (m : foobar) : int =
  let aggregate = fun (i, m : int * (int * int)) -> i + m.0 + m.1
  in Map.fold aggregate m 10

let deep_op (m: foobar) : foobar =
  let coco = 0, m in
  let coco = 0, Map.remove 42 coco.1 in
  let coco = 0, Map.update 32 (Some 16) coco.1
  in coco.1
type storage = int

type parameter =
  Add of int
| Sub of int

type return = operation list * storage

let main (action, store : parameter * storage) =
  let store =
    store +
      (match action with
         Add n -> n
       | Sub n -> -n)
  in ([] : operation list), store

let match_bool (b : bool) : int =
  match b with
    True -> 10
  | False -> 0

let match_list (l : int list) : int =
  match l with
    hd::tl -> hd
  | [] -> 10

let match_option (i : int option) : int =
  match i with
    Some n -> n
  | None -> 0
type storage = int

type parameter =
  Increment of int
| Decrement of int

type return = operation list * storage

let add (a : int) (b : int) : int = a + b
let sub (a : int) (b : int) : int = a - b

let main (action, store : parameter * storage) : return =
  let store =
    match action  with
      Increment n -> add store n
    | Decrement n -> sub store n
  in ([] : operation list), store
type t3 = { foo : int ; bar : nat ;  baz : string}

type param_r = t3 michelson_pair_right_comb
type param_l = t3 michelson_pair_left_comb

let main_r (action, store : param_r * unit) : (operation list * unit) =
 ([] : operation list),  unit

let main_l (action, store : param_l * unit) : (operation list * unit) =
 ([] : operation list),  unit
type foo = {
  bar : string;
  baz : nat;
}

let main2 (pm, s : union1_michelson * nat) =
  let p = union1_from_michelson pm in
  match p with
  | Choice1 f -> ([] : operation list), f.baz
  | Choice2 f -> ([] : operation list), f.baz

type st4 =
  | Foo4 of int
  | Bar4 of nat
  | Baz4 of string
  | Boz4 of bool

type st3 =
  | Foo3 of int
  | Bar3 of nat
  | Baz3 of string

(*convert from*)

type tr3 = (string,"baz4",bool,"boz4")michelson_or
type tr2 = (nat,"bar4",tr3,"") michelson_or
type tr1 = (int,"foo4",tr2,"")michelson_or
let vr : tr1 = M_right (M_right (M_left "eq":tr3):tr2)

type tl3 = (int,"foo4",nat,"bar4")michelson_or
type tl2 = (tl3,"",string,"baz4") michelson_or
type tl1 = (tl2,"",bool,"boz4")michelson_or
let vl : tl1 = M_left (M_right "eq":tl2)
// Test michelson insertion in CameLIGO

let michelson_add (n : nat * nat) : nat =
  [%Michelson ({| { UNPAIR;ADD } |} : nat * nat -> nat) ] n
type inner_storage = (int,"one",nat,"two") michelson_or
type storage = (int,"three",inner_storage,"four") michelson_or 

type return = operation list * storage

let main (action, store : unit * storage) : return =
  let foo = (M_right (M_left 1 : inner_storage) : storage) in
  (([] : operation list), (foo: storage))
type inner_storage = (int,"one",nat,"two") michelson_pair
type storage = (int,"three",inner_storage,"four") michelson_pair

type return = operation list * storage

let main (action, store : unit * storage) : return =
  let foo = (3,(1,2n)) in
  (([] : operation list), (foo: storage))(* Test function with several parameters *)

let abcde_curried (a : int)  (b : int)  (c : int)  (d : int)  (e : int) : int =
  c + e + 3

let abcde (x : int * int * int * int * int) : int =
  abcde_curried x.0 x.1 x.2 x.3 x.4
// storage type

type counter = nat
type threshold = nat
type authorized_keys = key list
type id = string

type storage = {
  id        : id;
  counter   : counter;
  threshold : threshold;
  auth      : authorized_keys
}

// I/O types

type message = unit -> operation list

type signatures = (key_hash * signature) list

type check_message_pt = {
  counter    : counter;
  message    : message;
  signatures : signatures
}

type return = operation list * storage

type parameter = CheckMessage of check_message_pt

let check_message (param, s : check_message_pt * storage) : return =
  let message : message = param.message in
  let s =
    if param.counter <> s.counter then
      (failwith "Counters does not match" : storage)
    else
      let packed_payload : bytes =
        Bytes.pack (message, param.counter, s.id, chain_id) in
      let valid : nat = 0n in
      let keys : authorized_keys = s.auth in
      let aux =
        fun (vk, pkh_sig: (nat * authorized_keys)*(key_hash * signature)) ->
          let valid, keys = vk in
            match keys with
            | [] -> vk
            | key::keys ->
               if pkh_sig.0 = Crypto.hash_key key
               then
                 let valid =
                   if Crypto.check key pkh_sig.1 packed_payload
                   then valid + 1n
                   else (failwith "Invalid signature" : nat)
                 in valid, keys
               else valid, keys in
      let valid, keys  =
        List.fold aux param.signatures (valid, keys) in
      if valid < s.threshold then
        (failwith ("Not enough signatures passed the check") : storage)
      else {s with counter = s.counter + 1n}
    in message unit, s

let main (action, store : parameter * storage) : return =
  match action with
    CheckMessage (p) -> check_message (p, store)
type foobar = int option

let s : foobar = Some 42
let n : foobar = None
type bls_l = (bls12_381_g1 * bls12_381_g2) list
type bool_option = bool option

let a = [%Michelson ({| 
  { PUSH @vk_gamma_c bls12_381_g1 0x063bd6e11e2fcaac1dd8cf68c6b1925a73c3c583e298ed37c41c3715115cf96358a42dbe85a0228cbfd8a6c8a8c54cd015b5ae2860d1cc47f84698d951f14d9448d03f04df2ca0ffe609a2067d6f1a892163a5e05e541279134cae52b1f23c6b; }
  |} : bls12_381_g1)]  

let b = [%Michelson ({| 
    { PUSH @vk_delta bls12_381_g2 0x10c6d5cdca84fc3c7f33061add256f48e0ab03a697832b338901898b650419eb6f334b28153fb73ad2ecd1cd2ac67053161e9f46cfbdaf7b1132a4654a55162850249650f9b873ac3113fa8c02ef1cd1df481480a4457f351d28f4da89d19fa405c3d77f686dc9a24d2681c9184bf2b091f62e6b24df651a3da8bd7067e14e7908fb02f8955b84af5081614cb5bc49b416d9edf914fc608c441b3f2eb8b6043736ddb9d4e4d62334a23b5625c14ef3e1a7e99258386310221b22d83a5eac035c; }
  |} : bls12_381_g2)]

let pairing_check (n : bls_l) : bool_option = [%Michelson ({| {PAIRING_CHECK; SOME} |} : bls_l -> bool_option)] n

let main (p, s : bls_l * bool_option) : operation list * bool_option =
 (([] : operation list), pairing_check p) type foobar = {foo : int; bar : int}

let fb : foobar = {foo=0; bar=0}

type abc = {a : int; b : int; c : int}

let abc : abc = {a=42; b=142; c=242}

let a : int = abc.a
let b : int = abc.b
let c : int = abc.c

let projection (r : foobar) : int = r.foo + r.bar

let modify (r : foobar) : foobar = {foo = 256; bar = r.bar}

let modify_abc (r : abc) : abc = let c = 42 in {r with b=2048; c=c}

type big_record = {
  a : int;
  b : int;
  c : int;
  d : int;
  e : int
}

let br : big_record = {
  a = 23;
  b = 23;
  c = 23;
  d = 23;
  e = 23
}

type double_record = {inner : abc}

let modify_inner (r : double_record) : double_record =
  {r with inner.b = 2048}
// Test while loops in PascaLIGO

let rec sum ((n,acc):int * int) : int =
    if (n < 1) then acc else sum (n-1, acc+n)
 
let rec fibo ((n,n_1,n_0):int*int*int) : int = 
    if (n < 2) then n_1 else fibo (n-1, n_1 + n_0, n_1)
type ss = 8 sapling_state

type storage = int * ss
type parameter = 8 sapling_transaction

type return = operation list * storage

let main (tr, store : parameter * storage) : return =
 ([] : operation list),
 (
    let es : ss = Tezos.sapling_empty_state in
    match Tezos.sapling_verify_update tr es with
   | Some x -> x
   | None -> (failwith "failed" : storage)
 )let main (p : unit) : address = Tezos.self_address
let y (_ : unit) : nat =
  let x : nat = 1n in
  begin
    (let x : nat = 2n in unit) ;
    (let x : nat = 23n in unit) ;
    (let x : nat = 42n in unit) ;
    x
  end
// Test set iteration

let aggregate (i : int) (j : int) : int = i + j

let fold_op (s : int set) : int = Set.fold aggregate s 15
(* Test set operations in CameLIGO *)

let literal_op (p: unit) : string set =
  Set.literal ["foo"; "bar"; "foobar"]

let add_op (s : string set) : string set =
   Set.add "foobar" s

let remove_op (s : string set) : string set =
   Set.remove "foobar" s

let remove_deep (s : string set * nat) : string set =
  Set.remove "foobar" s.0

(*
let patch_op (s: string set) : string set =
  begin patch s with set ["foobar"]; end with s

let patch_op_deep (s: string set * nat) : string set * nat =
  begin patch s.0 with set ["foobar"]; end with s
*)

let mem_op (s : string set) : bool = Set.mem "foobar" s

let size_op (s: string set) : nat = Set.cardinal s
let main (p : key_hash) : operation list =
  let useless : operation = Tezos.set_delegate (Some p)
  in ([] : operation list)
(* Test that the string concatenation syntax in CameLIGO works *)

let size_op (s : string) : nat = String.length s
let slice_op (s : string) : string = String.sub 1n 2n s
let concat_syntax (s : string) = s ^ "test_literal"
let main (ps : unit * unit) : operation list * unit =
  if true
  then (failwith "This contract always fails" : operation list * unit)
  else (failwith "This contract still always fails" : operation list * unit)
type parameter =
  Increment of int
| Decrement of int

type storage = int

type return = operation list * storage

let test_param = Increment 1
let test_storage = 2

let main (action, store : parameter * storage) : return =
  let store =
    match action with
    | Increment n -> store + n
    | Decrement n -> store - n
  in ([] : operation list), store
let add_tez : tez = 21mutez + 0.000_021tez
let sub_tez : tez = 0.000021tez - 0.000_020tez
let not_enough_tez : tez = 461_168_601_842_738_7903mutez

let add_more_tez : tez =
  100tez + 10tez + 1tez + 0.1tez + 0.01tez + 0.001tez
(*
Modelled after:

  https://gitlab.com/tezos/tezos/-/blob/95a072715b/tests_python/contracts_alpha/mini_scenarios/ticket_builder_fungible.tz

Goes with ticket_wallet.mligo.
*)

type mint_parameter =
  [@layout:comb]
  {destination : unit ticket contract;
   amount : nat}

type parameter =
  | Burn of unit ticket
  | Mint of mint_parameter

type storage =
  [@layout:comb]
  {admin : address}

let main (arg : parameter * storage) : operation list * storage =
  begin
    assert (Tezos.amount = 0mutez);
    let (p,s) = arg in
    match p with
    | Burn ticket ->
      begin
        let ((ticketer, _), ticket) = (Tezos.read_ticket ticket : (address * (unit * nat)) * unit ticket) in
        assert (ticketer = Tezos.self_address);
        (([] : operation list), s)
      end
    | Mint mint ->
      begin
        assert (Tezos.sender = s.admin);
        let ticket = Tezos.create_ticket () mint.amount in
        let op = Tezos.transaction ticket 0mutez mint.destination in
        ([op], s)
      end
  end
(*
Modelled after:

  https://gitlab.com/tezos/tezos/-/blob/95a072715b/tests_python/contracts_alpha/mini_scenarios/ticket_wallet_fungible.tz

Goes with ticket_builder.mligo.
*)

type send_parameter =
  [@layout:comb]
  {destination : unit ticket contract;
   amount : nat;
   ticketer : address}

type parameter =
  | Receive of unit ticket
  | Send of send_parameter

type storage =
  [@layout:comb]
  {manager : address;
   tickets : (address, unit ticket) big_map}

let main (arg : parameter * storage) : operation list * storage =
  begin
    assert (Tezos.amount = 0mutez);
    let (p,storage) = arg in
    let {manager = manager ; tickets = tickets } = storage in
    ( match p with
      | Receive ticket ->
        let ((ticketer,_), ticket) = Tezos.read_ticket ticket in
        let (old_ticket, tickets) = Big_map.get_and_update ticketer (None : unit ticket option) tickets in
        let ticket =
          match old_ticket with
          | None -> ticket
          | Some old_ticket -> (
            match Tezos.join_tickets (ticket, old_ticket) with
            | None -> (failwith "impossible?" : unit ticket)
            | Some joined -> joined
          )
        in
        let (_, tickets) = Big_map.get_and_update ticketer (Some ticket) tickets in
        (([] : operation list), {manager = manager; tickets = tickets})
      | Send send -> begin
        assert (Tezos.sender = manager) ;
        let (ticket, tickets) = Big_map.get_and_update send.ticketer (None : unit ticket option) tickets in
        ( match ticket with
          | None -> (failwith "no tickets" : operation list * storage)
          | Some ticket ->
            let ((_,(_,total_amt)), ticket) = Tezos.read_ticket ticket in
            let send_amt = send.amount in
            let keep_amt : nat =
              match is_nat (total_amt - send_amt) with
              | None -> (failwith "not enough tickets" : nat)
              | Some keep_amt -> keep_amt
            in
            ( match Tezos.split_ticket ticket (send_amt, keep_amt) with
              | None -> (failwith "impossible?" : operation list * storage)
              | Some split_tickets ->
                let (send_ticket,keep_ticket) = split_tickets in
                let (_, tickets) = Big_map.get_and_update send.ticketer (Some keep_ticket) tickets in
                let op = Tezos.transaction send_ticket 0mutez send.destination in
                ([op], {manager = manager; tickets = tickets})
            )
        )
      end
    )
  end
type parameter = unit

type storage = {
  next_use : timestamp;
  interval : int;
  execute  : unit -> operation list
}

type return = operation list * storage

let main (action, store : parameter * storage) : return =
  (* Multiple evaluations of Tezos.now give different values *)
  let my_now : timestamp = Tezos.now in
  if my_now > store.next_use
  then
    let store : storage =
      {store with next_use = my_now + store.interval}
    in store.execute (), store
  else
    (* TODO: Add the time until next use to this message *)
    (failwith "You have to wait before you can execute this contract again."
     : return)
type abc = int * int * int

let projection_abc (tpl : abc) : int = tpl.1

type foobar = int * int

let fb : foobar = (0,0)

let projection (tpl : foobar) : int = tpl.0 + tpl.1

type big_tuple = int * int * int * int * int

let br : big_tuple = (23, 23, 23, 23, 23)
let sum (result, i : int * int) : int = result - i
let parentheses ((((result, i))) : ((int * int))) : int = result - i
let g (b : int) = b + 3

let f (b : int * int) : int -> int = g

let a (b : int * int -> int -> int) : int = (b (5,3)) 5

let test1 (_: int) = a f

let n (a, b : int * int) : int = a + b

let o (p : int * int -> int) : int = p (3, 9)

let test2 (ignore : int) = o (n)
type foobar = int * int
let test_t : foobar = 10, 25
let foo, bar = test_t

let type_tuple_d (p : unit) = foo + bar

type complex = string * int * string * nat
let test_t_2 = "hello", 10, "world", 50n
let hello, ten, world, fifty_n = test_t_2

let type_tuple_d_2 (p : unit) = hello ^ world
type foobar =
| Foo of int
| Bar of bool
| Kee of nat

let foo : foobar = Foo 42
let bar : foobar = Bar true
let kee : foobar = Kee 23n
type storage = {
  title       : string;
  yea         : nat;
  nay         : nat;
  voters      : address set;
  start_time  : timestamp;
  finish_time : timestamp
}

type return = operation list * storage

type vote = Yea | Nay

type reset = {
  title       : string;
  start_time  : timestamp;
  finish_time : timestamp
}

type parameter =
| Vote  of vote
| Reset of reset

let reset (reset, _ : reset * storage) : return =
  ([] : operation list),
  {title       = reset.title;
   yea         = 0n;
   nay         = 0n;
   voters      = (Set.empty : address set);
   start_time  = reset.start_time;
   finish_time = reset.finish_time}

let vote (vote, store : vote * storage) : return =
  let now = Tezos.now in
  (* let _ =
     assert (now >= store.start_time && store.finish_time > now) in *)
  let addr = Tezos.sender in
  (* let _ = assert (not Set.mem addr store.voters) in *)
  let store =
    match vote with
      Yea -> {store with yea = store.yea + 1n}
    | Nay -> {store with nay = store.nay + 1n}
  in ([] : operation list),
     {store with voters = Set.add addr store.voters}

let main (action, store : parameter * storage) : return =
  match action with
  | Vote v  -> vote (v, store)
  | Reset r -> reset (r, store)
(* IF YOU CHANGE THIS, CHANGE THE EXAMPLE ON THE FRONT PAGE OF THE WEBSITE *)

type storage = int

(* variant defining pseudo multi-entrypoint actions *)

type parameter =
| Increment of int
| Decrement of int

let add (a,b: int * int) : int = a + b
let sub (a,b: int * int) : int = a - b

(* real entrypoint that re-routes the flow based on the parameter provided *)

let main (p,s: parameter * storage) =
 let storage =
   match p with
   | Increment n -> add (s, n)
   | Decrement n -> sub (s, n)
 in ([] : operation list), storage

(* IF YOU CHANGE THIS, CHANGE THE EXAMPLE ON THE FRONT PAGE OF THE WEBSITE *)
