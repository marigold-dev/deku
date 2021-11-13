module H=Helpers
module Ligo_proto = Environment.Protocols
open Trace
open Errors
open Ast_typed
open H

(*
  Each constant has its own type.

  LIGO's type-system is currently too
  weak to express the constant's type. For instance:
  - "ADD" has a special kind of type of polymorphism. If "ADD" gets two `int`s,
    it will return an `int`. If it gets two `nat`s, it will return a `nat`.
    Regular polymorphism wouldn't work because "ADD" only accepts `int`s or
    `nat`s.
  - "NONE" (from Some/None) requires an annotation.

  Instead of a LIGO type, constant types are representend as functions. These
  functions take as parameters:
  - The list of types of the arguments of the constants. When typing `2 + 2`,
    the types might be `[ int ; int ]`.
  - The expected type of the whole expression. It is optional. When typing
    `[] : list(operation)`, it will be `Some ( list (operation) )`. When
    typing `2 + 2` (with no additional context), it will be `None`.
  The output is the type of the whole expression. An error is returned through
  the Trace monad if it doesn't type-check (`"toto" + 42`).

  Various helpers are defined bellow.
*)


let none ~raise loc = typer_0 ~raise loc "NONE" @@ fun tv_opt ->
  match tv_opt with
  | None -> raise.raise (not_annotated loc)
  | Some t -> trace_option ~raise (expected_option loc t) @@ assert_t_option t; t

let set_empty ~raise loc = typer_0 ~raise loc "SET_EMPTY" @@ fun tv_opt ->
  match tv_opt with
  | None -> raise.raise (not_annotated loc)
  | Some t -> trace_option ~raise (expected_set loc t) @@ assert_t_set t ; t

let set_update ~raise loc = typer_3 ~raise loc "SET_UPDATE" @@ fun elt flag set ->
  let elt' = trace_option ~raise (expected_set loc set) @@ get_t_set set in
  let () = trace_option ~raise (expected_bool loc flag) @@ assert_t_bool flag in
  let () = assert_eq_1 ~raise ~loc elt elt' in
  set

let sub ~raise loc = typer_2 ~raise loc "SUB" @@ fun a b ->
  if eq_2 (a , b) (t_bls12_381_g1 ())
  then (t_bls12_381_g1 ()) else
  if eq_2 (a , b) (t_bls12_381_g2 ())
  then (t_bls12_381_g2 ()) else
  if eq_2 (a , b) (t_bls12_381_fr ())
  then (t_bls12_381_fr ()) else
  if (eq_1 a (t_int ()) || eq_1 a (t_nat ()))
  && (eq_1 b (t_int ()) || eq_1 b (t_nat ()))
  then t_int () else
  if (eq_2 (a , b) (t_timestamp ()))
  then t_int () else
  if (eq_1 a (t_timestamp ()) && eq_1 b (t_int ()))
  then t_timestamp () else
  if (eq_2 (a , b) (t_mutez ()))
  then t_mutez () else
    raise.raise (bad_subtraction loc)

let some ~raise loc = typer_1 ~raise loc "SOME" @@ fun a -> t_option a

let map_remove ~raise loc : typer = typer_2 ~raise loc "MAP_REMOVE" @@ fun k m ->
  let (src , _) = trace_option ~raise (expected_big_map loc m) @@
      Option.bind_eager_or (get_t_map m) (get_t_big_map m) in
  let () = assert_eq_1 ~raise ~loc src k in
  m

let map_empty ~raise loc = typer_0 ~raise loc "MAP_EMPTY" @@ fun tv_opt ->
  match tv_opt with
  | None -> raise.raise (not_annotated loc)
  | Some t ->
    let (src, dst) = trace_option ~raise (expected_map loc t) @@ get_t_map t in
    t_map src dst

let big_map_empty ~raise loc = typer_0 ~raise loc "BIG_MAP_EMPTY" @@ fun tv_opt ->
  match tv_opt with
  | None -> raise.raise (not_annotated loc)
  | Some t ->
    let (src, dst) = trace_option ~raise (expected_big_map loc t) @@ get_t_big_map t in
    t_big_map src dst

let map_add ~raise loc : typer = typer_3 loc ~raise "MAP_ADD" @@ fun k v m ->
  let (src , dst) = trace_option ~raise (expected_big_map loc m) @@
      Option.bind_eager_or (get_t_map m) (get_t_big_map m) in
  let () = assert_eq_1 ~raise ~loc src k in
  let () = assert_eq_1 ~raise ~loc dst v in
  m

let map_update ~raise loc : typer = typer_3 ~raise loc "MAP_UPDATE" @@ fun k v m ->
  let (src , dst) = trace_option ~raise (expected_big_map loc m) @@
      Option.bind_eager_or (get_t_map m) (get_t_big_map m) in
  let () = assert_eq_1 ~raise ~loc src k in
  let v' = trace_option ~raise (expected_option loc v) @@ get_t_option v in
  let () = assert_eq_1 ~raise ~loc dst v' in
  m

let map_mem ~raise loc : typer = typer_2 ~raise loc "MAP_MEM" @@ fun k m ->
  let (src , _) = trace_option ~raise (expected_big_map loc m) @@
      Option.bind_eager_or (get_t_map m) (get_t_big_map m) in
  let () = assert_eq_1 ~raise ~loc src k in
  t_bool ()

let map_find ~raise loc : typer = typer_2 ~raise loc "MAP_FIND" @@ fun k m ->
  let (src , dst) = trace_option ~raise (expected_big_map loc m) @@
      Option.bind_eager_or (get_t_map m) (get_t_big_map m) in
  let () = assert_eq_1 ~raise ~loc src k in
  dst

let map_find_opt ~raise loc : typer = typer_2 ~raise loc "MAP_FIND_OPT" @@ fun k m ->
  let (src , dst) = trace_option ~raise (expected_big_map loc m) @@
      Option.bind_eager_or (get_t_map m) (get_t_big_map m) in
  let () = assert_eq_1 ~raise ~loc src k in
  t_option dst

let map_iter ~raise loc : typer = typer_2 ~raise loc "MAP_ITER" @@ fun f m ->
  let (k, v) = trace_option ~raise (expected_map loc m) @@ get_t_map m in
  let (arg , res) = trace_option ~raise (expected_function loc f) @@ get_t_function f in
  let kv = t_pair k v in
  let unit = t_unit () in
  let () = assert_eq_1 ~raise ~loc arg kv in
  let () = assert_eq_1 ~raise ~loc res unit in
  t_unit ()

let map_map ~raise loc : typer = typer_2 ~raise loc "MAP_MAP" @@ fun f m ->
  let (k, v) = trace_option ~raise (expected_map loc m) @@ get_t_map m in
  let (arg , res) = trace_option ~raise (expected_function loc f) @@ get_t_function f in
  let kv = t_pair k v in
  let () = assert_eq_1 ~raise ~loc arg kv in
  t_map k res

let map_get_and_update ~raise loc : typer = typer_3 ~raise loc "MAP_GET_AND_UPDATE" @@ fun k opt_v m ->
  let v = trace_option ~raise (expected_option loc opt_v) @@ get_t_option opt_v in
  let (src , dst) = trace_option ~raise (expected_map loc m) @@ get_t_map m in
  let () = assert_eq_1 ~raise ~loc src k in
  let () = assert_eq_1 ~raise ~loc dst v in
  t_pair opt_v m

let big_map_get_and_update ~raise loc : typer = typer_3 ~raise loc "BIG_MAP_GET_AND_UPDATE" @@ fun k opt_v m ->
  let v = trace_option ~raise (expected_option loc opt_v) @@ get_t_option opt_v in
  let (src , dst) = trace_option ~raise (expected_map loc m) @@ get_t_big_map m in
  let () = assert_eq_1 ~raise ~loc src k in
  let () = assert_eq_1 ~raise ~loc dst v in
  t_pair opt_v m

let size ~raise loc = typer_1 ~raise loc "SIZE" @@ fun t ->
  let () =
    Assert.assert_true ~raise (wrong_size loc t) @@
    (is_t_map t || is_t_list t || is_t_string t || is_t_bytes t || is_t_set t ) in
  t_nat ()

let slice ~raise loc = typer_3 ~raise loc "SLICE" @@ fun i j s ->
  let t_nat = t_nat () in
  let () = assert_eq_1 ~raise ~loc i t_nat in
  let () = assert_eq_1 ~raise ~loc j t_nat in
  if eq_1 s (t_string ())
  then t_string ()
  else if eq_1 s (t_bytes ())
  then t_bytes ()
  else raise.raise @@ typeclass_error loc
      [
        [t_nat;t_nat;t_string()] ;
        [t_nat;t_nat;t_bytes()] ;
      ]
      [i ; j ; s]

let failwith_ ~raise loc = typer_1_opt ~raise loc "failwith" @@ fun t opt ->
  let _ =
    if eq_1 t (t_string ())
    then ()
    else if eq_1 t (t_nat ())
    then ()
    else if eq_1 t (t_int ())
    then ()
    else
      raise.raise @@ typeclass_error loc
        [
          [t_string()] ;
          [t_nat()] ;
          [t_int()] ;
        ]
        [t] in
  let default = t_unit () in
  Simple_utils.Option.value ~default opt

let int ~raise loc : typer = typer_1 ~raise loc "INT" @@ fun t ->
  if (eq_1 t (t_nat ()) || eq_1 t (t_bls12_381_fr ()))
  then (t_int ()) else
    raise.raise @@ typeclass_error loc
              [
                [t_bls12_381_fr()] ;
                [t_nat ()] ;
              ]
              [t]

let bytes_pack ~raise loc : typer = typer_1 ~raise loc "PACK" @@ fun _t ->
  t_bytes ()

let bytes_unpack ~raise loc = typer_1_opt ~raise loc "UNPACK" @@ fun input tv_opt ->
  let () = trace_option ~raise (expected_bytes loc input) @@ assert_t_bytes input in
  match tv_opt with
  | None -> raise.raise (not_annotated loc)
  | Some t ->
    let t = trace_option ~raise (expected_option loc t) @@ get_t_option t in
    t_option t

let hash256 ~raise loc = typer_1 ~raise loc "SHA256" @@ fun t ->
  let () = trace_option ~raise (expected_bytes loc t) @@ assert_t_bytes t in
  t_bytes ()

let hash512 ~raise loc = typer_1 ~raise loc "SHA512" @@ fun t ->
  let () = trace_option ~raise (expected_bytes loc t) @@ assert_t_bytes t in
  t_bytes ()

let blake2b ~raise loc = typer_1 ~raise loc "BLAKE2b" @@ fun t ->
  let () = trace_option ~raise (expected_bytes loc t) @@ assert_t_bytes t in
  t_bytes ()

let sha3 ~raise loc = typer_1 ~raise loc "SHA3" @@ fun t ->
  let () = trace_option ~raise (expected_bytes loc t) @@ assert_t_bytes t in
  t_bytes ()

let keccak ~raise loc = typer_1 ~raise loc "KECCAK" @@ fun t ->
  let () = trace_option ~raise (expected_bytes loc t) @@ assert_t_bytes t in
  t_bytes ()

let hash_key ~raise loc = typer_1 ~raise loc "HASH_KEY" @@ fun t ->
  let () = trace_option ~raise (expected_key loc t) @@ assert_t_key t in
  t_key_hash ()

let check_signature ~raise loc = typer_3 ~raise loc "CHECK_SIGNATURE" @@ fun k s b ->
  let () = trace_option ~raise (expected_key loc k) @@ assert_t_key k in
  let () = trace_option ~raise (expected_signature loc s) @@ assert_t_signature s in
  let () = trace_option ~raise (expected_bytes loc b) @@ assert_t_bytes b in
  t_bool ()

let sender ~raise loc = constant' ~raise loc "SENDER" @@ t_address ()

let source ~raise loc = constant' ~raise loc "SOURCE" @@ t_address ()

let unit ~raise loc = constant' ~raise loc "UNIT" @@ t_unit ()

let never ~raise loc = typer_1_opt ~raise loc "NEVER" @@ fun nev tv_opt ->
  let () = assert_eq_1 ~raise ~loc nev (t_never ()) in
  match tv_opt with
  | None -> raise.raise (not_annotated loc)
  | Some t -> t

let amount ~raise loc = constant' ~raise loc "AMOUNT" @@ t_mutez ()

let balance ~raise loc = constant' ~raise loc "BALANCE" @@ t_mutez ()

let chain_id ~raise loc = constant' ~raise loc "CHAIN_ID" @@ t_chain_id ()

let level ~raise loc = constant' ~raise loc "LEVEL" @@ t_nat ()

let total_voting_power ~raise loc = constant' ~raise loc "TOTAL_VOTING_POWER" @@ t_nat ()

let voting_power ~raise loc = typer_1 ~raise loc "VOTING_POWER" @@ fun t ->
  let () = trace_option ~raise (expected_key_hash loc t) @@ assert_t_key_hash t in
  t_nat ()

let address ~raise loc = typer_1 ~raise loc "ADDRESS" @@ fun c ->
  let () = trace_option ~raise (expected_contract loc c) @@ assert_t_contract c in
  t_address ()

let self_address ~raise loc = typer_0 ~raise loc "SELF_ADDRESS" @@ fun _ ->
  t_address ()

let self ~raise loc = typer_1_opt ~raise loc "SELF" @@ fun entrypoint_as_string tv_opt ->
  let () = trace_option ~raise (expected_string loc entrypoint_as_string) @@ assert_t_string entrypoint_as_string in
  match tv_opt with
  | None -> raise.raise (not_annotated loc)
  | Some t -> t

let implicit_account ~raise loc = typer_1 ~raise loc "IMPLICIT_ACCOUNT" @@ fun key_hash ->
  let () = trace_option ~raise (expected_key_hash loc key_hash) @@ assert_t_key_hash key_hash in
  t_contract (t_unit () )

let now ~raise loc = constant' ~raise loc "NOW" @@ t_timestamp ()
let ctrue ~raise loc = constant' ~raise loc "TRUE" @@ t_bool ()
let cfalse ~raise loc = constant' ~raise loc "FALSE" @@ t_bool ()

let transaction ~raise loc = typer_3 ~raise loc "CALL" @@ fun param amount contract ->
  let () = trace_option ~raise (expected_mutez loc amount) @@ assert_t_mutez amount in
  let contract_param = trace_option ~raise (expected_contract loc contract) @@ get_t_contract contract in
  let () = assert_eq_1 ~raise ~loc param contract_param in
  t_operation ()

let create_contract ~raise loc = typer_4 ~raise loc "CREATE_CONTRACT" @@ fun f kh_opt amount init_storage  ->
  let (args , ret) = trace_option ~raise (expected_function loc f) @@ get_t_function f in
  let (_,s) = trace_option ~raise (expected_pair loc args) @@ get_t_pair args in
  let (oplist,s') = trace_option ~raise (expected_pair loc ret) @@ get_t_pair ret in
  let () = trace_option ~raise (expected_mutez loc amount) @@ assert_t_mutez amount in
  let (delegate) = trace_option ~raise (expected_option loc kh_opt) @@ get_t_option kh_opt in
  let () = assert_eq_1 ~raise ~loc s s' in
  let () = assert_eq_1 ~raise ~loc s init_storage in
  let () = trace_option ~raise (expected_op_list loc oplist) @@ assert_t_list_operation oplist in
  let () = trace_option ~raise (expected_key_hash loc delegate) @@ assert_t_key_hash delegate in
  t_pair (t_operation ()) (t_address ())

let get_contract ~raise loc = typer_1_opt ~raise loc "CONTRACT" @@ fun addr_tv tv_opt ->
  let t_addr = t_address () in
  let () = assert_eq_1 ~raise ~loc addr_tv t_addr in
  let tv = trace_option ~raise (not_annotated loc) tv_opt in
  let tv' = trace_option ~raise (expected_contract loc tv) @@ get_t_contract tv in
  t_contract tv'

let get_contract_with_error ~raise loc = typer_2_opt ~raise loc "CONTRACT" @@ fun addr_tv err_str tv_opt ->
  let t_addr = t_address () in
  let () = assert_eq_1 ~raise ~loc addr_tv t_addr in
  let tv = trace_option ~raise (contract_not_annotated loc) tv_opt in
  let tv' = trace_option ~raise (expected_contract loc tv) @@ get_t_contract tv in
  let () = trace_option ~raise (expected_string loc err_str) @@ get_t_string err_str in
  t_contract tv'

let get_contract_opt ~raise loc = typer_1_opt ~raise loc "CONTRACT OPT" @@ fun addr_tv tv_opt ->
  let t_addr = t_address () in
  let () = assert_eq_1 ~raise ~loc addr_tv t_addr in
  let tv = trace_option ~raise (contract_not_annotated loc) tv_opt in
  let tv = trace_option ~raise (expected_option loc tv) @@ get_t_option tv in
  let tv' = trace_option ~raise (expected_contract loc tv) @@ get_t_contract tv in
  t_option (t_contract tv')

let get_entrypoint ~raise loc = typer_2_opt ~raise loc "CONTRACT_ENTRYPOINT" @@ fun entry_tv addr_tv tv_opt ->
  let t_string = t_string () in
  let t_addr = t_address () in
  let () = assert_eq_1 ~raise ~loc entry_tv t_string in
  let () = assert_eq_1 ~raise ~loc addr_tv t_addr in
  let tv = trace_option ~raise (not_annotated loc) tv_opt in
  let tv' = trace_option ~raise (expected_contract loc tv) @@ get_t_contract tv in
  t_contract tv'

let get_entrypoint_opt ~raise loc = typer_2_opt ~raise loc "CONTRACT_ENTRYPOINT_OPT" @@ fun entry_tv addr_tv tv_opt ->
  let t_string = t_string () in
  let t_addr = t_address () in
  let () = assert_eq_1 ~raise ~loc entry_tv t_string in
  let () = assert_eq_1 ~raise ~loc addr_tv t_addr in
  let tv = trace_option ~raise (contract_not_annotated loc) tv_opt in
  let tv = trace_option ~raise (expected_option loc tv) @@ get_t_option tv in
  let tv' = trace_option ~raise (expected_contract loc tv) @@ get_t_contract tv in
  t_option (t_contract tv' )

let set_delegate ~raise loc = typer_1 ~raise loc "SET_DELEGATE" @@ fun delegate_opt ->
  let kh_opt = (t_option (t_key_hash ()) ) in
  let () = assert_eq_1 ~raise ~loc delegate_opt kh_opt in
  t_operation ()

let abs ~raise loc = typer_1 ~raise loc "ABS" @@ fun t ->
  let () = trace_option ~raise (expected_int loc t) @@ assert_t_int t in
  t_nat ()

let is_nat ~raise loc = typer_1 ~raise loc "ISNAT" @@ fun t ->
  let () = trace_option ~raise (expected_int loc t) @@ assert_t_int t in
  t_option (t_nat ())

let neg ~raise loc = typer_1 ~raise loc "NEG" @@ fun t ->
  let () = Assert.assert_true ~raise (wrong_neg loc t) @@ (eq_1 t (t_nat ()) || eq_1 t (t_int ())) in
  t_int ()

let unopt ~raise loc = typer_1 ~raise loc "ASSERT" @@ fun a ->
  let a  = trace_option ~raise (expected_option loc a) @@ get_t_option a in
  a

let unopt_with_error ~raise loc = typer_2 ~raise loc "ASSERT" @@ fun a b ->
  let a  = trace_option ~raise (expected_option loc a) @@ get_t_option a in
  let () = trace_option ~raise (expected_option loc a) @@ assert_t_string b in
  a

let assertion ~raise loc = typer_1 ~raise loc "ASSERT" @@ fun a ->
  let () = trace_option ~raise (expected_bool loc a) @@ assert_t_bool a in
  t_unit ()

let assertion_with_error ~raise loc = typer_2 ~raise loc "ASSERT_WITH_ERROR" @@ fun a b ->
  let () = trace_option ~raise (expected_bool loc a) @@ assert_t_bool a in
  let () = trace_option ~raise (expected_string loc b) @@ assert_t_string b in
  t_unit ()

let assert_some ~raise loc = typer_1 ~raise loc "ASSERT_SOME" @@ fun a ->
  let () = trace_option ~raise (expected_option loc a) @@ assert_t_option a in
  t_unit ()

let assert_some_with_error ~raise loc = typer_2 ~raise loc "ASSERT_SOME_WITH_ERROR" @@ fun a b ->
  let () = trace_option ~raise (expected_option loc a) @@ assert_t_option a in
  let () = trace_option ~raise (expected_string loc b) @@ assert_t_string b in
  t_unit ()

let times ~raise loc = typer_2 ~raise loc "TIMES" @@ fun a b ->
  if (eq_1 a (t_bls12_381_g1 ()) && eq_1 b (t_bls12_381_fr ()))
  then (t_bls12_381_g1 ()) else
  if (eq_1 a (t_bls12_381_g2 ()) && eq_1 b (t_bls12_381_fr ()))
  then (t_bls12_381_g2 ()) else
  if (eq_1 a (t_bls12_381_fr ()) && eq_1 b (t_bls12_381_fr ()))
  then (t_bls12_381_fr ()) else
  if (eq_1 a (t_nat ()) && eq_1 b (t_bls12_381_fr ()))
  then (t_bls12_381_fr ()) else
  if (eq_1 a (t_int ()) && eq_1 b (t_bls12_381_fr ()))
  then (t_bls12_381_fr ()) else
  if (eq_1 a (t_bls12_381_fr ()) && eq_1 b (t_nat ()))
  then (t_bls12_381_fr ()) else
  if (eq_1 a (t_bls12_381_fr ()) && eq_1 b (t_int ()))
  then (t_bls12_381_fr ()) else
  if eq_2 (a , b) (t_nat ())
  then t_nat () else
  if eq_2 (a , b) (t_int ())
  then t_int () else
  if (eq_1 a (t_nat ()) && eq_1 b (t_mutez ())) || (eq_1 b (t_nat ()) && eq_1 a (t_mutez ()))
  then t_mutez () else
  if (eq_1 a (t_nat ()) && eq_1 b (t_int ())) || (eq_1 b (t_nat ()) && eq_1 a (t_int ()))
  then t_int () else
    raise.raise @@ typeclass_error loc
              [
                [t_bls12_381_g1();t_bls12_381_g1()] ;
                [t_bls12_381_g2();t_bls12_381_g2()] ;
                [t_bls12_381_fr();t_bls12_381_fr()] ;
                [t_nat();t_bls12_381_fr()] ;
                [t_int();t_bls12_381_fr()] ;
                [t_bls12_381_fr();t_nat()] ;
                [t_bls12_381_fr();t_int()] ;
                [t_nat();t_nat()] ;
                [t_int();t_int()] ;
                [t_nat();t_mutez()] ;
                [t_mutez();t_nat()] ;
                [t_nat();t_int()] ;
                [t_int();t_nat()] ;
              ]
              [a; b]

let ediv ~raise loc = typer_2 ~raise loc "EDIV" @@ fun a b ->
  if eq_2 (a , b) (t_nat ())
  then t_option (t_pair (t_nat ()) (t_nat ()) ) else
  if eq_2 (a , b) (t_int ())
  then t_option (t_pair (t_int ()) (t_nat ()) ) else
  if eq_1 a (t_nat ()) && eq_1 b (t_int ())
  then t_option (t_pair (t_int ()) (t_nat ()) ) else
  if eq_1 a (t_int ()) && eq_1 b (t_nat ())
  then t_option (t_pair (t_int ()) (t_nat ()) ) else
  if eq_1 a (t_mutez ()) && eq_1 b (t_mutez ())
  then t_option (t_pair (t_nat ()) (t_mutez ()) ) else
  if eq_1 a (t_mutez ()) && eq_1 b (t_nat ())
  then t_option (t_pair (t_mutez ()) (t_mutez ()) ) else
    raise.raise @@ typeclass_error loc
      [
        [t_nat();t_nat()] ;
        [t_int();t_int()] ;
        [t_nat();t_int()] ;
        [t_int();t_nat()] ;
        [t_mutez();t_nat()] ;
        [t_mutez();t_mutez()] ;
      ]
      [a; b]

let div ~raise loc = typer_2 ~raise loc "DIV" @@ fun a b ->
  if eq_2 (a , b) (t_nat ())
  then t_nat () else
  if eq_2 (a , b) (t_int ())
  then t_int () else
  if eq_1 a (t_int ()) && eq_1 b (t_nat ())
  then t_int () else
  if eq_1 a (t_nat ()) && eq_1 b (t_int ())
  then t_int () else
  if eq_1 a (t_mutez ()) && eq_1 b (t_nat ())
  then t_mutez () else
  if eq_1 a (t_mutez ()) && eq_1 b (t_mutez ())
  then t_nat () else
    raise.raise @@ typeclass_error loc
      [
        [t_nat();t_nat()] ;
        [t_int();t_int()] ;
        [t_nat();t_int()] ;
        [t_int();t_nat()] ;
        [t_mutez();t_nat()] ;
        [t_mutez();t_mutez()] ;
      ]
      [a; b]

let mod_ ~raise loc = typer_2 ~raise loc "MOD" @@ fun a b ->
  if (eq_1 a (t_nat ()) || eq_1 a (t_int ())) && (eq_1 b (t_nat ()) || eq_1 b (t_int ()))
  then t_nat () else
  if eq_1 a (t_mutez ()) && eq_1 b (t_mutez ())
  then t_mutez () else
  if eq_1 a (t_mutez ()) && eq_1 b (t_nat ())
  then t_mutez () else
    raise.raise @@ typeclass_error loc
      [
        [t_nat();t_nat()] ;
        [t_nat();t_int()] ;
        [t_int();t_nat()] ;
        [t_int();t_int()] ;
        [t_mutez();t_nat()] ;
        [t_mutez();t_mutez()] ;
      ]
      [a; b]

let add ~raise loc = typer_2 ~raise loc "ADD" @@ fun a b ->
  if eq_2 (a , b) (t_bls12_381_g1 ())
  then (t_bls12_381_g1 ()) else
  if eq_2 (a , b) (t_bls12_381_g2 ())
  then (t_bls12_381_g2 ()) else
  if eq_2 (a , b) (t_bls12_381_fr ())
  then (t_bls12_381_fr ()) else
  if eq_2 (a , b) (t_nat ())
  then t_nat () else
  if eq_2 (a , b) (t_int ())
  then t_int () else
  if eq_2 (a , b) (t_mutez ())
  then t_mutez () else
  if (eq_1 a (t_nat ()) && eq_1 b (t_int ())) || (eq_1 b (t_nat ()) && eq_1 a (t_int ()))
  then t_int () else
  if (eq_1 a (t_timestamp ()) && eq_1 b (t_int ())) || (eq_1 b (t_timestamp ()) && eq_1 a (t_int ()))
  then t_timestamp () else
    raise.raise @@ typeclass_error loc
              [ 
                [t_bls12_381_g1();t_bls12_381_g1()] ;
                [t_bls12_381_g2();t_bls12_381_g2()] ;
                [t_bls12_381_fr();t_bls12_381_fr()] ;
                [t_nat();t_nat()] ;
                [t_int();t_int()] ;
                [t_mutez();t_mutez()] ;
                [t_nat();t_int()] ;
                [t_int();t_nat()] ;
                [t_timestamp();t_int()] ;
                [t_int();t_timestamp()] ;
              ]
              [a; b]

let polymorphic_add ~raise loc = typer_2 ~raise loc "POLYMORPHIC_ADD" @@ fun a b ->
  if eq_2 (a , b) (t_string ())
  then t_string () else
  if eq_2 (a , b) (t_bls12_381_g1 ())
  then (t_bls12_381_g1 ()) else
  if eq_2 (a , b) (t_bls12_381_g2 ())
  then (t_bls12_381_g2 ()) else
  if eq_2 (a , b) (t_bls12_381_fr ())
  then (t_bls12_381_fr ()) else
  if eq_2 (a , b) (t_nat ())
  then t_nat () else
  if eq_2 (a , b) (t_int ())
  then t_int () else
  if eq_2 (a , b) (t_mutez ())
  then t_mutez () else
  if (eq_1 a (t_nat ()) && eq_1 b (t_int ())) || (eq_1 b (t_nat ()) && eq_1 a (t_int ()))
  then t_int () else
  if (eq_1 a (t_timestamp ()) && eq_1 b (t_int ())) || (eq_1 b (t_timestamp ()) && eq_1 a (t_int ()))
  then t_timestamp () else
    raise.raise @@ typeclass_error loc
              [ 
                [t_string();t_string()] ;
                [t_bls12_381_g1();t_bls12_381_g1()] ;
                [t_bls12_381_g2();t_bls12_381_g2()] ;
                [t_bls12_381_fr();t_bls12_381_fr()] ;
                [t_nat();t_nat()] ;
                [t_int();t_int()] ;
                [t_mutez();t_mutez()] ;
                [t_nat();t_int()] ;
                [t_int();t_nat()] ;
                [t_timestamp();t_int()] ;
                [t_int();t_timestamp()] ;
              ]
              [a; b]

let set_mem ~raise loc = typer_2 ~raise loc "SET_MEM" @@ fun elt set ->
  let key = trace_option ~raise (expected_set loc set) @@ get_t_set set in
  let () = assert_eq_1 ~raise ~loc elt key in
  t_bool ()

let set_add ~raise loc = typer_2 ~raise loc "SET_ADD" @@ fun elt set ->
  let key = trace_option ~raise (expected_set loc set) @@ get_t_set set in
  let () = assert_eq_1 ~raise ~loc elt key in
  set

let set_remove ~raise loc = typer_2 ~raise loc "SET_REMOVE" @@ fun elt set ->
  let key = trace_option ~raise (expected_set loc set) @@ get_t_set set in
  let () = assert_eq_1 ~raise ~loc elt key in
  set

let set_iter ~raise loc = typer_2 ~raise loc "SET_ITER" @@ fun body set ->
  let (arg , res) = trace_option ~raise (expected_function loc body) @@ get_t_function body in
  let () = assert_eq_1 ~raise ~loc res (t_unit ()) in
  let key = trace_option ~raise (expected_set loc set) @@ get_t_set set in
  let () = assert_eq_1 ~raise ~loc key arg in
  (t_unit ())

let list_empty ~raise loc = typer_0 ~raise loc "LIST_EMPTY" @@ fun tv_opt ->
  match tv_opt with
  | None -> raise.raise (not_annotated loc)
  | Some t ->
    let () = trace_option ~raise (expected_list loc t) @@ assert_t_list t in
    t

let list_iter ~raise loc = typer_2 ~raise loc "LIST_ITER" @@ fun body lst ->
  let (arg , res) = trace_option ~raise (expected_function loc body) @@ get_t_function body in
  let () = assert_eq_1 ~raise ~loc res (t_unit ()) in
  let key = trace_option ~raise (expected_list loc lst) @@ get_t_list lst in
  let () = assert_eq_1 ~raise ~loc key arg in
  (t_unit ())

let list_map ~raise loc = typer_2 ~raise loc "LIST_MAP" @@ fun body lst ->
  let (arg , res) = trace_option ~raise (expected_function loc body) @@ get_t_function body in
  let key = trace_option ~raise (expected_list loc lst) @@ get_t_list lst in
  let () = assert_eq_1 ~raise ~loc key arg in
  (t_list res )

let fold ~raise loc = typer_3 ~raise loc "FOLD" @@ fun body container init ->
  let (arg , res) = trace_option ~raise (expected_function loc body) @@ get_t_function body in
  let (prec , cur) = trace_option ~raise (expected_pair loc arg) @@ get_t_pair arg in
  let key = trace_option ~raise (expected_list loc container) @@ Option.map_pair_or (get_t_list,get_t_set) container in
  let () = assert_eq_1 ~raise ~loc key cur in
  let () = assert_eq_1 ~raise ~loc prec res in
  let () = assert_eq_1 ~raise ~loc res init in
  res
let list_fold ~raise loc = typer_3 ~raise loc "LIST_FOLD" @@ fun body lst init ->
  let (arg , res) = trace_option ~raise (expected_function loc body) @@ get_t_function body in
  let (prec , cur) = trace_option ~raise (expected_pair loc arg) @@ get_t_pair arg in
  let key = trace_option ~raise (expected_list loc lst) @@ get_t_list lst in
  let () = assert_eq_1 ~raise ~loc key cur in
  let () = assert_eq_1 ~raise ~loc prec res in
  let () = assert_eq_1 ~raise ~loc res init in
  res

let list_fold_left ~raise loc = typer_3 ~raise loc "LIST_FOLD_LEFT" @@ fun body init lst ->
  let (arg , res) = trace_option ~raise (expected_function loc body) @@ get_t_function body in
  let (prec , cur) = trace_option ~raise (expected_pair loc arg) @@ get_t_pair arg in
  let key = trace_option ~raise (expected_list loc lst) @@ get_t_list lst in
  let () = assert_eq_1 ~raise ~loc key cur in
  let () = assert_eq_1 ~raise ~loc prec res in
  let () = assert_eq_1 ~raise ~loc res init in
  res

let list_fold_right ~raise loc = typer_3 ~raise loc "LIST_FOLD_RIGHT" @@ fun body lst init ->
  let (arg , res) = trace_option ~raise (expected_function loc body) @@ get_t_function body in
  let (cur , prec) = trace_option ~raise (expected_pair loc arg) @@ get_t_pair arg in
  let key = trace_option ~raise (expected_list loc lst) @@ get_t_list lst in
  let () = assert_eq_1 ~raise ~loc key cur in
  let () = assert_eq_1 ~raise ~loc prec res in
  let () = assert_eq_1 ~raise ~loc res init in
  res

let list_head_opt ~raise loc = typer_1 ~raise loc "LIST_HEAD_OPT" @@ fun lst ->
  let key = trace_option ~raise (expected_list loc lst) @@ get_t_list lst in
  t_option ~loc key

let list_tail_opt ~raise loc = typer_1 ~raise loc "LIST_TAIL_OPT" @@ fun lst ->
  let key = trace_option ~raise (expected_list loc lst) @@ get_t_list lst in
  t_option ~loc @@ t_list ~loc key

let set_fold ~raise loc = typer_3 ~raise loc "SET_FOLD" @@ fun body lst init ->
  let (arg , res) = trace_option ~raise (expected_function loc body) @@ get_t_function body in
  let (prec , cur) = trace_option ~raise (expected_pair loc arg) @@ get_t_pair arg in
  let key = trace_option ~raise (expected_set loc lst) @@ get_t_set lst in
  let () = assert_eq_1 ~raise ~loc key cur in
  let () = assert_eq_1 ~raise ~loc prec res in
  let () = assert_eq_1 ~raise ~loc res init in
  res

let set_fold_desc ~raise loc = typer_3 ~raise loc "SET_FOLD_DESC" @@ fun body lst init ->
  let (arg , res) = trace_option ~raise (expected_function loc body) @@ get_t_function body in
  let (cur , prec) = trace_option ~raise (expected_pair loc arg) @@ get_t_pair arg in
  let key = trace_option ~raise (expected_set loc lst) @@ get_t_set lst in
  let () = assert_eq_1 ~raise ~loc key cur in
  let () = assert_eq_1 ~raise ~loc prec res in
  let () = assert_eq_1 ~raise ~loc res init in
  res

let map_fold ~raise loc = typer_3 ~raise loc "MAP_FOLD" @@ fun body map init ->
  let (arg , res) = trace_option ~raise (expected_function loc body) @@ get_t_function body in
  let (prec , cur) = trace_option ~raise (expected_pair loc arg) @@ get_t_pair arg in
  let (key , value) = trace_option ~raise (expected_map loc map) @@ get_t_map map in
  let kv = t_pair key value in
  let () = assert_eq_1 ~raise ~loc kv cur in
  let () = assert_eq_1 ~raise ~loc prec res in
  let () = assert_eq_1 ~raise ~loc res init in
  res

(** FOLD_WHILE is a fold operation that takes an initial value of a certain type
    and then iterates on it until a condition is reached. The auxillary function
    that does the fold returns either boolean true or boolean false to indicate
    whether the fold should continue or not. Necessarily then the initial value
    must match the input parameter of the auxillary function, and the auxillary
    should return type (bool * input) *)
let fold_while ~raise loc = typer_2 ~raise loc "FOLD_WHILE" @@ fun body init ->
  let (arg, result) = trace_option ~raise (expected_function loc body) @@ get_t_function body in
  let () = assert_eq_1 ~raise ~loc arg init in
  let () = assert_eq_1 ~raise ~loc (t_pair (t_bool ()) init) result
  in init

(* Continue and Stop are just syntactic sugar for building a pair (bool * a') *)
let continue ~raise loc = typer_1 ~raise loc "CONTINUE" @@ fun arg ->
  t_pair (t_bool ()) arg

let stop ~raise loc = typer_1 ~raise loc "STOP" @@ fun arg ->
  (t_pair (t_bool ()) arg)

let not_ ~raise loc = typer_1 ~raise loc "NOT" @@ fun elt ->
  if eq_1 elt (t_bool ())
  then t_bool ()
  else if eq_1 elt (t_nat ()) || eq_1 elt (t_int ())
  then t_int ()
  else raise.raise @@ wrong_not loc elt

let or_ ~raise loc = typer_2 ~raise loc "OR" @@ fun a b ->
  if eq_2 (a , b) (t_bool ())
  then t_bool ()
  else if eq_2 (a , b) (t_nat ())
  then t_nat ()
  else raise.raise @@ typeclass_error loc
      [
        [t_bool();t_bool()] ;
        [t_nat();t_nat()] ;
      ]
      [a; b]

let xor ~raise loc = typer_2 ~raise loc "XOR" @@ fun a b ->
  if eq_2 (a , b) (t_bool ())
  then t_bool ()
  else if eq_2 (a , b) (t_nat ())
  then t_nat ()
  else raise.raise @@ typeclass_error loc
      [
        [t_bool();t_bool()] ;
        [t_nat();t_nat()] ;
      ]
      [a; b]

let and_ ~raise loc = typer_2 ~raise loc "AND" @@ fun a b ->
  if eq_2 (a , b) (t_bool ())
  then t_bool ()
  else if eq_2 (a , b) (t_nat ()) || (eq_1 b (t_nat ()) && eq_1 a (t_int ()))
  then t_nat ()
  else raise.raise @@ typeclass_error loc
      [
        [t_bool();t_bool()] ;
        [t_nat();t_nat()] ;
        [t_int();t_nat()] ;
      ]
      [a; b]

let lsl_ ~raise loc = typer_2 ~raise loc "LSL" @@ fun a b ->
  if eq_2 (a , b) (t_nat ())
  then t_nat ()
  else raise.raise @@ typeclass_error loc
      [
        [t_nat();t_nat()] ;
      ]
      [a; b]

let lsr_ ~raise loc = typer_2 ~raise loc "LSR" @@ fun a b ->
  if eq_2 (a , b) (t_nat ())
  then t_nat ()
  else raise.raise @@ typeclass_error loc
      [
        [t_nat();t_nat()] ;
      ]
      [a; b]

let concat ~raise loc = typer_2 ~raise loc "CONCAT" @@ fun a b ->
  if eq_2 (a , b) (t_string ())
  then t_string ()
  else if eq_2 (a , b) (t_bytes ())
  then t_bytes ()
  else raise.raise @@ typeclass_error loc
      [
        [t_string();t_string()] ;
        [t_bytes();t_bytes()] ;
      ]
      [a; b]

let cons ~raise loc = typer_2 ~raise loc "CONS" @@ fun hd tl ->
  let elt = trace_option ~raise (expected_list loc tl) @@ get_t_list tl in
  let () = assert_eq_1 ~raise ~loc hd elt in
  tl

let simple_comparator ~raise : Location.t -> string -> typer = fun loc s -> typer_2 ~raise loc s @@ fun a b ->
  let () =
    Assert.assert_true ~raise (uncomparable_types loc a b) @@
    List.exists ~f:(eq_2 (a , b)) [
      t_address () ;
      t_bool () ;
      t_bytes () ;
      t_chain_id ();
      t_int () ;
      t_key () ;
      t_key_hash () ;
      t_mutez () ;
      t_nat () ;
      t_signature ();
      t_string () ;
      t_timestamp () ;
      t_unit ();
      t_never ();
      t_michelson_code () ;
    ] in
  t_bool ()

let rec record_comparator ~raise ~test : Location.t -> string -> typer = fun loc s -> typer_2 ~raise loc s @@ fun a b ->
  let () =
    Assert.assert_true ~raise (uncomparable_types loc a b) @@ eq_1 a b
  in
  let a_r =
    trace_option ~raise (comparator_composed loc a) @@
    get_t_record a in
  let b_r = trace_option ~raise (expected_variant loc b) @@ get_t_record b in
  let aux a b : type_expression =
    comparator ~raise ~test loc s [a.associated_type;b.associated_type] None
  in
  let _ = List.map2_exn ~f:aux (LMap.to_list a_r.content) (LMap.to_list b_r.content) in
  t_bool ()

and sum_comparator ~raise ~test : Location.t -> string -> typer = fun loc s -> typer_2 ~raise loc s @@ fun a b ->
  let () =
    Assert.assert_true ~raise (uncomparable_types loc a b) @@ eq_1 a b
  in
  let a_r =
    trace_option ~raise (comparator_composed loc a) @@
    get_t_sum a in
  let b_r = trace_option ~raise (expected_variant loc b) @@ get_t_sum b in
  let aux a b : type_expression =
    comparator ~raise ~test loc s [a.associated_type;b.associated_type] None
  in
  let _ = List.map2_exn ~f:aux (LMap.to_list a_r.content) (LMap.to_list b_r.content) in
  t_bool ()

and list_comparator ~raise ~test : Location.t -> string -> typer = fun loc s -> typer_2 ~raise loc s @@ fun a_lst b_lst ->
  let () =
    Assert.assert_true ~raise (uncomparable_types loc a_lst b_lst) @@ eq_1 a_lst b_lst
  in
  let a =
    trace_option ~raise (comparator_composed loc a_lst) @@
    get_t_list a_lst in
  let b = trace_option ~raise (expected_option loc b_lst) @@ get_t_list b_lst in
  comparator ~raise ~test loc s [a;b] None

and set_comparator ~raise ~test : Location.t -> string -> typer = fun loc s -> typer_2 ~raise loc s @@ fun a_set b_set ->
  let () =
    Assert.assert_true ~raise (uncomparable_types loc a_set b_set) @@ eq_1 a_set b_set
  in
  let a =
    trace_option ~raise (comparator_composed loc a_set) @@
    get_t_set a_set in
  let b = trace_option ~raise (expected_option loc b_set) @@ get_t_set b_set in
  comparator ~raise ~test loc s [a;b] None

and map_comparator ~raise ~test : Location.t -> string -> typer = fun loc s -> typer_2 ~raise loc s @@ fun a_map b_map ->
  let () =
    Assert.assert_true ~raise (uncomparable_types loc a_map b_map) @@ eq_1 a_map b_map
  in
  let (a_key, a_value) =
    trace_option ~raise (comparator_composed loc a_map) @@
    get_t_map a_map in
  let (b_key, b_value) = trace_option ~raise (expected_option loc b_map) @@ get_t_map b_map in
  let _ = comparator ~raise ~test loc s [a_key;b_key] None in
  let _ = comparator ~raise ~test loc s [a_value;b_value] None in
  t_bool ()

and big_map_comparator ~raise ~test : Location.t -> string -> typer = fun loc s -> typer_2 ~raise loc s @@ fun a_map b_map ->
  let () =
    Assert.assert_true ~raise (uncomparable_types loc a_map b_map) @@ eq_1 a_map b_map
  in
  let (a_key, a_value) =
    trace_option ~raise (comparator_composed loc a_map) @@
    get_t_big_map a_map in
  let (b_key, b_value) = trace_option ~raise (expected_option loc b_map) @@ get_t_big_map b_map in
  let _ = comparator ~raise ~test loc s [a_key;b_key] None in
  let _ = comparator ~raise ~test loc s [a_value;b_value] None in
  t_bool ()

and option_comparator ~raise ~test : Location.t -> string -> typer = fun loc s -> typer_2 ~raise loc s @@ fun a_opt b_opt ->
  let () =
    Assert.assert_true ~raise (uncomparable_types loc a_opt b_opt) @@ eq_1 a_opt b_opt
  in
  let a =
    trace_option ~raise (comparator_composed loc a_opt) @@
    get_t_option a_opt in
  let b = trace_option ~raise (expected_option loc b_opt) @@ get_t_option b_opt in
  comparator ~raise ~test loc s [a;b] None

and comparator ~raise ~test : Location.t -> string -> typer = fun loc s -> typer_2 ~raise loc s @@ fun a b ->
  if test
  then
    bind_exists ~raise @@ List.Ne.of_list [list_comparator ~test loc s [a;b] None;
                                           set_comparator ~test loc s [a;b] None;
                                           map_comparator ~test loc s [a;b] None;
                                           simple_comparator loc s [a;b] None;
                                           option_comparator ~test loc s [a;b] None;
                                           record_comparator ~test loc s [a;b] None;
                                           sum_comparator ~test loc s [a;b] None;
                                           big_map_comparator ~test loc s [a;b] None]
  else
    bind_exists ~raise @@ List.Ne.of_list [simple_comparator loc s [a;b] None;
                                           option_comparator ~test loc s [a;b] None;
                                           record_comparator ~test loc s [a;b] None;
                                           sum_comparator ~test loc s [a;b] None]

let ticket ~raise loc = typer_2 ~raise loc "TICKET" @@ fun dat amt ->
  let () = assert_eq_1 ~raise ~loc amt (t_nat ()) in
  t_ticket dat

let read_ticket ~raise loc = typer_1 ~raise loc "READ_TICKET" @@ fun ticket ->
  let payload = trace_option ~raise (expected_ticket loc ticket) @@ get_t_ticket ticket in
  t_pair (t_pair (t_address ()) (t_pair payload (t_nat ()))) ticket

let split_ticket ~raise loc = typer_2 ~raise loc "SPLIT_TICKET" @@ fun ticket amts ->
  let t_nat = t_nat () in
  let (a,b) = trace_option ~raise (expected_pair loc amts) @@ get_t_pair amts in
  let () = assert_eq_1 ~raise ~loc a t_nat in
  let () = assert_eq_1 ~raise ~loc b t_nat in
  let _ = trace_option ~raise (expected_ticket loc ticket) @@ get_t_ticket ticket in
  t_option (t_pair ticket ticket)

let join_ticket ~raise loc = typer_1 ~raise loc "JOIN_TICKET" @@ fun ticks ->
  let (ticka,tickb) = trace_option ~raise (expected_pair loc ticks) @@ get_t_pair ticks in
  let data = trace_option ~raise (expected_ticket loc ticka) @@ get_t_ticket ticka in
  let datb = trace_option ~raise (expected_ticket loc tickb) @@ get_t_ticket tickb in
  let () = assert_eq_1 ~raise ~loc data datb in
  t_option ticka

let pairing_check ~raise loc = typer_1 ~raise loc "PAIRING_CHECK" @@ fun lst ->
  let p = trace_option ~raise (expected_list loc lst) @@ get_t_list lst in
  let (g1,g2) = trace_option ~raise (expected_list loc p) @@ get_t_pair p in
  let () = assert_eq_1 ~raise ~loc g1 (t_bls12_381_g1 ()) in (*TODO expected_tbls .. ? *)
  let () = assert_eq_1 ~raise ~loc g2 (t_bls12_381_g2 ()) in
  (t_bool ())

let sapling_verify_update ~raise loc = typer_2 ~raise loc "SAPLING_VERIFY_UPDATE" @@ fun tr state ->
  let singleton_tr = trace_option ~raise (expected_sapling_transaction loc tr) @@ get_t_sapling_transaction tr in
  let singleton_state = trace_option ~raise (expected_sapling_state loc state) @@ get_t_sapling_state state in
  let () = assert_eq_1 ~raise ~loc singleton_tr singleton_state in
  (t_option (t_pair (t_int ()) state))

let sapling_empty_state ~raise loc = typer_0 ~raise loc "SAPLING_EMPTY_STATE" @@ fun tv_opt ->
  trace_option ~raise (not_annotated loc) @@ tv_opt

let open_chest ~raise loc = typer_3 ~raise loc "OPEN_CHEST" @@ fun key chest n ->
  let () = assert_eq_1 ~raise ~loc key (t_chest_key ()) in
  let () = assert_eq_1 ~raise ~loc chest (t_chest ()) in
  let () = trace_option ~raise (expected_nat loc n) @@ get_t_nat n in
  t_chest_opening_result ()

let test_originate ~raise loc = typer_3 ~raise loc "TEST_ORIGINATE" @@ fun main storage balance ->
  let in_ty,_ = trace_option ~raise (expected_function loc main) @@ get_t_function main in
  let param_ty,storage_ty = trace_option ~raise (expected_pair loc in_ty) @@ get_t_pair in_ty in
  let () = assert_eq_1 ~raise ~loc balance (t_mutez ()) in
  let () = assert_eq_1 ~raise ~loc storage storage_ty in
  (t_triplet (t_typed_address param_ty storage_ty) (t_michelson_code ()) (t_int ()))

let test_state_reset ~raise loc = typer_2 ~raise loc "TEST_STATE_RESET" @@ fun n amts ->
  let amt = trace_option ~raise (expected_list loc amts) @@ get_t_list amts in
  let () = trace_option ~raise (expected_mutez loc amt) @@ get_t_mutez amt in
  let () = trace_option ~raise (expected_nat loc n) @@ get_t_nat n in
  (t_unit ())

let test_bootstrap_contract ~raise loc = typer_3 ~raise loc "TEST_BOOTSTRAP_CONTRACT" @@ fun balance main storage ->
  let in_ty,_ = trace_option ~raise (expected_function loc main) @@ get_t_function main in
  let _,storage_ty = trace_option ~raise (expected_pair loc in_ty) @@ get_t_pair in_ty in
  let () = assert_eq_1 ~raise ~loc balance (t_mutez ()) in
  let () = assert_eq_1 ~raise ~loc storage storage_ty in
  (t_unit ())

let test_nth_bootstrap_contract ~raise loc = typer_1 ~raise loc "TEST_NTH_BOOTSTRAP_CONTRACT" @@ fun n ->
  let () = assert_eq_1 ~raise ~loc n (t_nat ()) in
  (t_address ())

let test_set_now ~raise loc = typer_1 ~raise loc "TEST_SET_NOW" @@ fun time ->
  let () = assert_eq_1 ~raise ~loc time (t_timestamp ()) in
  (t_unit ())

let test_set_source ~raise loc = typer_1 ~raise loc "TEST_SET" @@ fun s ->
  let () = assert_eq_1 ~raise ~loc s (t_address ()) in
  (t_unit ())

let test_get_nth ~raise loc = typer_1 ~raise loc "TEST_GET_NTH" @@ fun n ->
  let () = trace_option ~raise (expected_int loc n) @@ assert_t_int n in
  (t_address ())

let test_external_call_to_contract_exn ~raise loc = typer_3 ~raise loc "TEST_EXTERNAL_CALL_TO_CONTRACT_EXN" @@ fun addr p amt  ->
  let contract_ty = trace_option ~raise (expected_contract loc addr) @@ get_t_contract addr in
  let () = assert_eq_1 ~raise ~loc amt (t_mutez ()) in
  let () = assert_eq_1 ~raise ~loc p contract_ty in
  (t_unit ())

let test_external_call_to_contract ~raise loc = typer_3 ~raise loc "TEST_EXTERNAL_CALL_TO_CONTRACT" @@ fun addr p amt  ->
  let contract_ty = trace_option ~raise (expected_contract loc addr) @@ get_t_contract addr in
  let () = assert_eq_1 ~raise ~loc amt (t_mutez ()) in
  let () = assert_eq_1 ~raise ~loc p contract_ty in
  (t_test_exec_result ())

let test_external_call_to_address_exn ~raise loc = typer_3 ~raise loc "TEST_EXTERNAL_CALL_TO_ADDRESS_EXN" @@ fun addr p amt  ->
  let () = assert_eq_1 ~raise ~loc addr (t_address ()) in
  let () = assert_eq_1 ~raise ~loc amt (t_mutez ()) in
  let () = assert_eq_1 ~raise ~loc p (t_michelson_code ()) in
  (t_unit ())

let test_external_call_to_address ~raise loc = typer_3 ~raise loc "TEST_EXTERNAL_CALL_TO_ADDRESS" @@ fun addr p amt  ->
  let () = assert_eq_1 ~raise ~loc addr (t_address ()) in
  let () = assert_eq_1 ~raise ~loc amt (t_mutez ()) in
  let () = assert_eq_1 ~raise ~loc p (t_michelson_code ()) in
  (t_test_exec_result ())

let test_get_storage ~raise loc = typer_1 ~raise loc "TEST_GET_STORAGE" @@ fun c ->
  let (_, storage_ty) = trace_option ~raise (expected_typed_address loc c) @@ get_t_typed_address c in
  storage_ty

let test_get_storage_of_address ~raise loc = typer_1 ~raise loc "TEST_GET_STORAGE_OF_ADDRESS" @@ fun addr ->
  let () = assert_eq_1 ~raise ~loc addr (t_address ()) in
  (t_michelson_code ())

let test_get_balance ~raise loc = typer_1 ~raise loc "TEST_GET_BALANCE" @@ fun addr ->
  let () = assert_eq_1 ~raise ~loc addr (t_address ()) in
  (t_mutez ())

let test_michelson_equal ~raise loc = typer_2 ~raise loc "TEST_ASSERT_EQUAL" @@ fun x y ->
  let () = trace_option ~raise (expected_michelson_code loc x) @@ assert_t_michelson_code x in
  let () = trace_option ~raise (expected_michelson_code loc y) @@ assert_t_michelson_code y in
  (t_bool ())

let test_log ~raise loc = typer_1 ~raise loc "TEST_LOG" @@ fun _ -> t_unit ()

let test_last_originations ~raise loc = typer_1 ~raise loc "TEST_LAST_ORIGINATIONS" @@ fun u ->
  let () = trace_option ~raise (expected_unit loc u) @@ assert_t_unit u in
  (t_map (t_address ()) (t_list (t_address ())))

let test_compile_meta_value ~raise loc = typer_1 ~raise loc "TEST_LAST_ORIGINATIONS" @@ fun _ ->
  (t_michelson_code ())

let test_mutate_value ~raise loc = typer_2 ~raise loc "TEST_MUTATE_VALUE" @@ fun n expr ->
  let () = assert_eq_1 ~raise ~loc n (t_nat ()) in
  (t_option (t_pair expr (t_mutation ())))

let test_mutation_test ~raise loc = typer_2 ~raise loc "TEST_MUTATION_TEST" @@ fun expr tester ->
  let (arg , res) = trace_option ~raise (expected_function loc tester) @@ get_t_function tester in
  let () = assert_eq_1 ~raise ~loc arg expr in
  (t_option (t_pair res (t_mutation ())))

let test_mutation_test_all ~raise loc = typer_2 ~raise loc "TEST_MUTATION_TEST_ALL" @@ fun expr tester ->
  let (arg , res) = trace_option ~raise (expected_function loc tester) @@ get_t_function tester in
  let () = assert_eq_1 ~raise ~loc arg expr in
  (t_list (t_pair res (t_mutation ())))

let test_save_mutation ~raise loc = typer_2 ~raise loc "TEST_SAVE_MUTATION" @@ fun dir mutation ->
  let () = assert_eq_1 ~raise ~loc mutation (t_mutation ()) in
  let () = assert_eq_1 ~raise ~loc dir (t_string ()) in
  (t_option (t_string ()))

let test_run ~raise loc = typer_2 ~raise loc "TEST_RUN" @@ fun _ _ ->
  (t_michelson_code ())

let test_eval ~raise loc = typer_1 ~raise loc "TEST_EVAL" @@ fun _ ->
  (t_michelson_code ())

let test_to_contract ~raise loc = typer_1 ~raise loc "TEST_TO_CONTRACT" @@ fun t ->
  let param_ty, _ = trace_option ~raise (expected_typed_address loc t) @@
                       get_t_typed_address t in
  let param_ty = Option.value (Ast_typed.Helpers.get_entrypoint "default" param_ty) ~default:param_ty in
  (t_contract param_ty)

let test_nth_bootstrap_typed_address ~raise loc = typer_1_opt ~raise loc "TEST_NTH_BOOTSTRAP_TYPED_ADDRESS" @@ fun nat tv_opt ->
  let () = trace_option ~raise (expected_nat loc nat) @@ get_t_nat nat in
  let tv = trace_option ~raise (not_annotated loc) tv_opt in
  let tv_parameter, tv_storage = trace_option ~raise (expected_option loc tv) @@ get_t_typed_address tv in
  (t_typed_address tv_parameter tv_storage)

let test_to_entrypoint ~raise loc = typer_2_opt ~raise loc "TEST_TO_ENTRYPOINT" @@ fun entry_tv contract_tv tv_opt ->
  let t_string = t_string () in
  let () = assert_eq_1 ~raise ~loc entry_tv t_string in
  let _ = trace_option ~raise (expected_contract loc contract_tv) @@
             get_t_typed_address contract_tv in
  let tv = trace_option ~raise (not_annotated loc) tv_opt in
  let tv' = trace_option ~raise (expected_contract loc tv) @@ get_t_contract tv in
  t_contract tv'

let test_to_typed_address ~raise loc = typer_1_opt ~raise loc "TEST_TO_TYPED_ADDRESS" @@ fun contract_tv tv_opt ->
  let parameter_ty = trace_option ~raise (expected_contract loc contract_tv) @@
             get_t_contract contract_tv in
  let tv = trace_option ~raise (not_annotated loc) tv_opt in
  let (parameter_ty', storage_ty) = trace_option ~raise (expected_contract loc tv) @@ get_t_typed_address tv in
  let () = assert_eq_1 ~raise ~loc parameter_ty parameter_ty' in
  t_typed_address parameter_ty storage_ty

let test_set_big_map ~raise loc = typer_2 ~raise loc "TEST_SET_BIG_MAP" @@ fun id bm ->
  let () = assert_eq_1 ~raise ~loc id (t_int ()) in
  let _ = trace_option ~raise (expected_big_map loc bm) @@ get_t_big_map bm in
  t_unit ()

let test_originate_from_file ~protocol_version ~raise loc =
  match (protocol_version : Ligo_proto.t) with
  | Edo ->
    typer_4 ~raise loc "TEST_ORIGINATE_FROM_FILE" @@ fun source_file entrypoint storage balance ->
      let () = trace_option ~raise (expected_string loc source_file) @@ assert_t_string source_file in
      let () = trace_option ~raise (expected_string loc entrypoint) @@ assert_t_string entrypoint in
      let () = trace_option ~raise (expected_michelson_code loc storage) @@ assert_t_michelson_code storage in
      let () = assert_eq_1 ~raise ~loc balance (t_mutez ()) in
      (t_triplet (t_address ()) (t_michelson_code ()) (t_int ()))
  | Hangzhou ->
    typer_5 ~raise loc "TEST_ORIGINATE_FROM_FILE" @@ fun source_file entrypoint views storage balance ->
      let tlist = trace_option ~raise (expected_list loc views) @@ get_t_list views in
      let () = trace_option ~raise (expected_string loc tlist) @@ assert_t_string tlist in
      let () = trace_option ~raise (expected_string loc source_file) @@ assert_t_string source_file in
      let () = trace_option ~raise (expected_string loc entrypoint) @@ assert_t_string entrypoint in
      let () = trace_option ~raise (expected_michelson_code loc storage) @@ assert_t_michelson_code storage in
      let () = assert_eq_1 ~raise ~loc balance (t_mutez ()) in
      (t_triplet (t_address ()) (t_michelson_code ()) (t_int ()))

let test_compile_contract ~raise loc = typer_1 ~raise loc "TEST_COMPILE_CONTRACT" @@ fun _ ->
  (t_michelson_code ())

let test_cast_address ~raise loc = typer_1_opt ~raise loc "TEST_CAST_ADDRESS" @@ fun addr tv_opt ->
  let cast_t = trace_option ~raise (not_annotated loc) @@ tv_opt in
  let (pty,sty) = trace_option ~raise (expected_typed_address loc cast_t) @@ get_t_typed_address cast_t in
  let () = trace_option ~raise (expected_address loc addr) @@ get_t_address addr in
  t_typed_address pty sty

let test_create_chest ~raise loc = typer_2 ~raise loc "TEST_CREATE_CHEST" @@ fun payload time ->
  let () = trace_option ~raise (expected_bytes loc payload) @@ get_t_bytes payload in
  let () = trace_option ~raise (expected_nat loc time) @@ get_t_nat time in
  t_pair (t_chest ()) (t_chest_key ())

let test_create_chest_key ~raise loc = typer_2 ~raise loc "TEST_CREATE_CHEST_KEY" @@ fun chest time ->
  let () = assert_eq_1 ~raise ~loc (t_chest ()) chest in
  let () = trace_option ~raise (expected_nat loc time) @@ get_t_nat time in
  (t_chest_key ())

let view ~raise loc = typer_3_opt ~raise loc "TEST_VIEW" @@ fun name _arg addr tv_opt ->
  let () = trace_option ~raise (expected_string loc name) @@ get_t_string name in
  let () = trace_option ~raise (expected_address loc addr) @@ get_t_address addr in
  let view_ret_t = trace_option ~raise (not_annotated loc) @@ tv_opt in
  let _ : type_expression = trace_option ~raise (expected_option loc view_ret_t) @@ get_t_option view_ret_t in
  view_ret_t

let constant_typers ~raise ~test ~protocol_version loc c : typer = match c with
  | C_INT                 -> int ~raise loc ;
  | C_UNIT                -> unit ~raise loc ;
  | C_NEVER               -> never ~raise loc ;
  | C_NOW                 -> now ~raise loc ;
  | C_TRUE                -> ctrue ~raise loc ;
  | C_FALSE               -> cfalse ~raise loc ;
  | C_IS_NAT              -> is_nat ~raise loc ;
  | C_SOME                -> some ~raise loc ;
  | C_NONE                -> none ~raise loc ;
  | C_UNOPT               -> unopt ~raise loc ;
  | C_UNOPT_WITH_ERROR    -> unopt_with_error ~raise loc ;
  | C_ASSERTION           -> assertion ~raise loc ;
  | C_ASSERTION_WITH_ERROR-> assertion_with_error ~raise loc ;
  | C_ASSERT_SOME         -> assert_some ~raise loc ;
  | C_ASSERT_SOME_WITH_ERROR -> assert_some_with_error ~raise loc ;
  | C_FAILWITH            -> failwith_ ~raise loc ;
    (* LOOPS *)
  | C_FOLD_WHILE          -> fold_while ~raise loc ;
  | C_FOLD_CONTINUE       -> continue ~raise loc ;
  | C_FOLD_STOP           -> stop ~raise loc ;
  | C_FOLD                -> fold ~raise loc ;
   (* MATH *)
  | C_NEG                 -> neg ~raise loc ;
  | C_ABS                 -> abs ~raise loc ;
  | C_ADD                 -> add ~raise loc ;
  | C_SUB                 -> sub ~raise loc ;
  | C_MUL                 -> times ~raise loc ;
  | C_EDIV                -> ediv ~raise loc ;
  | C_DIV                 -> div ~raise loc ;
  | C_MOD                 -> mod_ ~raise loc ;
    (* LOGIC *)
  | C_NOT                 -> not_ ~raise loc ;
  | C_AND                 -> and_ ~raise loc ;
  | C_OR                  -> or_ ~raise loc ;
  | C_XOR                 -> xor ~raise loc ;
  | C_LSL                 -> lsl_ ~raise loc;
  | C_LSR                 -> lsr_ ~raise loc;
    (* COMPARATOR *)
  | C_EQ                  -> comparator ~raise ~test loc "EQ" ;
  | C_NEQ                 -> comparator ~raise ~test loc "NEQ" ;
  | C_LT                  -> comparator ~raise ~test loc "LT" ;
  | C_GT                  -> comparator ~raise ~test loc "GT" ;
  | C_LE                  -> comparator ~raise ~test loc "LE" ;
  | C_GE                  -> comparator ~raise ~test loc "GE" ;
    (* BYTES / STRING *)
  | C_SIZE                -> size ~raise loc ;
  | C_CONCAT              -> concat ~raise loc ;
  | C_SLICE               -> slice ~raise loc ;
  | C_BYTES_PACK          -> bytes_pack ~raise loc ;
  | C_BYTES_UNPACK        -> bytes_unpack ~raise loc ;
    (* SET  *)
  | C_SET_EMPTY           -> set_empty ~raise loc;
  | C_SET_ADD             -> set_add ~raise loc ;
  | C_SET_REMOVE          -> set_remove ~raise loc ;
  | C_SET_ITER            -> set_iter ~raise loc ;
  | C_SET_FOLD            -> set_fold ~raise loc ;
  | C_SET_FOLD_DESC       -> set_fold_desc ~raise loc ;
  | C_SET_MEM             -> set_mem ~raise loc ;
  | C_SET_UPDATE          -> set_update ~raise loc;
  (* LIST *)
  | C_CONS                -> cons ~raise loc ;
  | C_LIST_EMPTY          -> list_empty ~raise loc;
  | C_LIST_ITER           -> list_iter ~raise loc ;
  | C_LIST_MAP            -> list_map ~raise loc ;
  | C_LIST_FOLD           -> list_fold ~raise loc ;
  | C_LIST_FOLD_LEFT      -> list_fold_left ~raise loc ;
  | C_LIST_FOLD_RIGHT     -> list_fold_right ~raise loc ;
  | C_LIST_HEAD_OPT       -> list_head_opt ~raise loc;
  | C_LIST_TAIL_OPT       -> list_tail_opt ~raise loc;
    (* MAP *)
  | C_MAP_EMPTY           -> map_empty ~raise loc;
  | C_BIG_MAP_EMPTY       -> big_map_empty ~raise loc;
  | C_MAP_ADD             -> map_add ~raise loc ;
  | C_MAP_REMOVE          -> map_remove ~raise loc ;
  | C_MAP_UPDATE          -> map_update ~raise loc ;
  | C_MAP_ITER            -> map_iter ~raise loc ;
  | C_MAP_MAP             -> map_map ~raise loc ;
  | C_MAP_FOLD            -> map_fold ~raise loc ;
  | C_MAP_MEM             -> map_mem ~raise loc ;
  | C_MAP_FIND            -> map_find ~raise loc ;
  | C_MAP_FIND_OPT        -> map_find_opt ~raise loc ;
  | C_MAP_GET_AND_UPDATE -> map_get_and_update ~raise loc ;
  (* BIG MAP *)
  | C_BIG_MAP_GET_AND_UPDATE -> big_map_get_and_update ~raise loc;
  (* CRYPTO *)
  | C_SHA256              -> hash256 ~raise loc ;
  | C_SHA512              -> hash512 ~raise loc ;
  | C_BLAKE2b             -> blake2b ~raise loc ;
  | C_HASH_KEY            -> hash_key ~raise loc ;
  | C_CHECK_SIGNATURE     -> check_signature ~raise loc ;
  | C_CHAIN_ID            -> chain_id ~raise loc;
  (* BLOCKCHAIN *)
  | C_CONTRACT            -> get_contract ~raise loc ;
  | C_CONTRACT_WITH_ERROR -> get_contract_with_error ~raise loc ;
  | C_CONTRACT_OPT        -> get_contract_opt ~raise loc ;
  | C_CONTRACT_ENTRYPOINT -> get_entrypoint ~raise loc ;
  | C_CONTRACT_ENTRYPOINT_OPT -> get_entrypoint_opt ~raise loc ;
  | C_AMOUNT              -> amount ~raise loc ;
  | C_BALANCE             -> balance ~raise loc ;
  | C_CALL                -> transaction ~raise loc ;
  | C_SENDER              -> sender ~raise loc ;
  | C_SOURCE              -> source ~raise loc ;
  | C_ADDRESS             -> address ~raise loc ;
  | C_SELF                -> self ~raise loc ;
  | C_SELF_ADDRESS        -> self_address ~raise loc ;
  | C_IMPLICIT_ACCOUNT    -> implicit_account ~raise loc ;
  | C_SET_DELEGATE        -> set_delegate ~raise loc ;
  | C_CREATE_CONTRACT     -> create_contract ~raise loc ;
  | C_SHA3              -> sha3 ~raise loc ;
  | C_KECCAK            -> keccak ~raise loc ;
  | C_LEVEL             -> level ~raise loc ;
  | C_VOTING_POWER      -> voting_power ~raise loc ;
  | C_TOTAL_VOTING_POWER -> total_voting_power ~raise loc ;
  | C_TICKET -> ticket ~raise loc ;
  | C_READ_TICKET -> read_ticket ~raise loc ;
  | C_SPLIT_TICKET -> split_ticket ~raise loc ;
  | C_JOIN_TICKET -> join_ticket ~raise loc ;
  | C_PAIRING_CHECK -> pairing_check ~raise loc ;
  | C_SAPLING_VERIFY_UPDATE -> sapling_verify_update ~raise loc ;
  | C_SAPLING_EMPTY_STATE -> sapling_empty_state ~raise loc ;
  | C_OPEN_CHEST -> open_chest ~raise loc ;
  | C_VIEW -> view ~raise loc ;
  (* TEST *)
  | C_TEST_ORIGINATE -> test_originate ~raise loc ;
  | C_TEST_SET_NOW -> test_set_now ~raise loc ;
  | C_TEST_SET_SOURCE -> test_set_source ~raise loc ;
  | C_TEST_SET_BAKER -> test_set_source ~raise loc ;
  | C_TEST_EXTERNAL_CALL_TO_CONTRACT -> test_external_call_to_contract ~raise loc ;
  | C_TEST_EXTERNAL_CALL_TO_CONTRACT_EXN -> test_external_call_to_contract_exn ~raise loc ;
  | C_TEST_EXTERNAL_CALL_TO_ADDRESS -> test_external_call_to_address ~raise loc ;
  | C_TEST_EXTERNAL_CALL_TO_ADDRESS_EXN -> test_external_call_to_address_exn ~raise loc ;
  | C_TEST_GET_STORAGE -> test_get_storage ~raise loc ;
  | C_TEST_GET_STORAGE_OF_ADDRESS -> test_get_storage_of_address ~raise loc ;
  | C_TEST_GET_BALANCE -> test_get_balance ~raise loc ;
  | C_TEST_MICHELSON_EQUAL -> test_michelson_equal ~raise loc ;
  | C_TEST_GET_NTH_BS -> test_get_nth ~raise loc ;
  | C_TEST_LOG -> test_log ~raise loc ;
  | C_TEST_STATE_RESET -> test_state_reset ~raise loc ;
  | C_TEST_BOOTSTRAP_CONTRACT -> test_bootstrap_contract ~raise loc ;
  | C_TEST_NTH_BOOTSTRAP_CONTRACT -> test_nth_bootstrap_contract ~raise loc ;
  | C_TEST_LAST_ORIGINATIONS -> test_last_originations ~raise loc ;
  | C_TEST_COMPILE_META_VALUE -> test_compile_meta_value ~raise loc ;
  | C_TEST_MUTATE_VALUE -> test_mutate_value ~raise loc ;
  | C_TEST_MUTATION_TEST -> test_mutation_test ~raise loc ;
  | C_TEST_MUTATION_TEST_ALL -> test_mutation_test_all ~raise loc ;
  | C_TEST_RUN -> test_run ~raise loc ;
  | C_TEST_EVAL -> test_eval ~raise loc ;
  | C_TEST_COMPILE_CONTRACT -> test_compile_contract ~raise loc ;
  | C_TEST_TO_CONTRACT -> test_to_contract ~raise loc ;
  | C_TEST_NTH_BOOTSTRAP_TYPED_ADDRESS -> test_nth_bootstrap_typed_address ~raise loc ;
  | C_TEST_TO_ENTRYPOINT -> test_to_entrypoint ~raise loc ;
  | C_TEST_TO_TYPED_ADDRESS -> test_to_typed_address ~raise loc ;
  | C_TEST_SET_BIG_MAP -> test_set_big_map ~raise loc ;
  | C_TEST_ORIGINATE_FROM_FILE -> test_originate_from_file ~protocol_version ~raise loc ;
  | C_TEST_SAVE_MUTATION -> test_save_mutation ~raise loc ;
  | C_TEST_CAST_ADDRESS -> test_cast_address ~raise loc;
  | C_TEST_CREATE_CHEST -> (
    match protocol_version with
    | Ligo_proto.Hangzhou -> test_create_chest ~raise loc ;
    | Ligo_proto.Edo ->
      raise.raise @@ corner_case (
        Format.asprintf "Unsupported constant %a in protocol %s"
        PP.constant' c
        (Ligo_proto.variant_to_string protocol_version)
      )
  )
  | C_TEST_CREATE_CHEST_KEY -> (
    match protocol_version with
    | Ligo_proto.Hangzhou -> test_create_chest_key ~raise loc ;
    | Ligo_proto.Edo ->
      raise.raise @@ corner_case (
        Format.asprintf "Unsupported constant %a in protocol %s"
          PP.constant' c
          (Ligo_proto.variant_to_string protocol_version)
      )
  )
  (* JsLIGO *)
  | C_POLYMORPHIC_ADD  -> polymorphic_add ~raise loc ;
  | _ as cst -> raise.raise (corner_case @@ Format.asprintf "typer not implemented for constant %a" PP.constant' cst)
