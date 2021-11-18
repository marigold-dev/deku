(* This test check that the type are comparable *)

let address_ (a: address) = a < a
let bool_ (a: bool) = a < a
let bytes_ (a: bytes) = a < a
let chain_id_ (a: chain_id) = a < a
let int_ (a: int) = a < a
let key_ (a: key) = a < a
let key_hash_ (a: key_hash) = a < a
let mutez_ (a: tez) = a < a
let nat_ (a: nat) = a < a
let option_ (a: int option) = a < a
let never_ (a: never) = a < a

(*
type toto = A of int
let sum_ (a : toto) = a < a
*)

type comp_pair = int * int

let comp_pair (a: comp_pair) = a < a

let signature_ (a: signature) = a < a
let string_ (a: string) = a < a
let timestamp_ (a: timestamp) = a < a
let unit_ (a: unit) = a < a
(*
type uncomp_pair_1 = int * int * int

let uncomp_pair_1 (a: uncomp_pair_1) = a < a

type uncomp_pair_2 = comp_pair * int

let uncomp_pair_2 (a: uncomp_pair_2) = a < a
*)
type inner_record = (int,"one",nat,"two") michelson_pair
type comb_record = (int,"three",inner_record,"four") michelson_pair

let comb_record (a : comb_record) = a < a
