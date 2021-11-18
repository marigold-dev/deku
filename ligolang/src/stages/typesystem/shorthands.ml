open Ast_core.Types
open Core
open Ast_core.Misc
open Ast_core.Reasons

(* TODO: remove this () argument, it is just here to make sure that
   the ~bound and ~constraints arguments are given (while adding the
   fields to the record, we need to make sure they're supplied
   everywhere) *)
let tc description ~bound ~constraints () type_vars allowed_list : type_constraint = {
    c = C_typeclass { tc_bound=bound; tc_constraints=constraints; tc_args = type_vars ; original_id = None ; typeclass = allowed_list} ;
    reason = "typeclass for operator: " ^ description
  }

let forall binder f =
  let () = ignore binder in
  let freshvar = fresh_type_variable () in
  let var = wrap Forall @@ P_variable freshvar in
  let body = f var in
  wrap Forall @@ P_forall { binder = freshvar ; constraints = [] ; body }

let forall_tc binder f =
  let () = ignore binder in
  let freshvar = fresh_type_variable () in
  let var = wrap Forall_TC @@ P_variable freshvar in
  let (tc, ty) = f var in
  wrap Forall_TC @@ P_forall { binder = freshvar ; constraints = tc ; body = ty }

(* chained forall *)
let forall2 a b f =
  forall a @@ fun a' ->
  forall b @@ fun b' ->
  f a' b'

let forall3 a b c f =
  forall a @@ fun a' ->
  forall b @@ fun b' ->
  forall c @@ fun c' ->
  f a' b' c'

let forall4 a b c d f =
  forall a @@ fun a' ->
  forall b @@ fun b' ->
  forall c @@ fun c' ->
  forall d @@ fun d' ->
  f a' b' c' d'

let forall4_tc a b c d f =
  forall    a @@ fun a' ->
  forall    b @@ fun b' ->
  forall    c @@ fun c' ->
  forall_tc d @@ fun d' ->
  f a' b' c' d'

let forall3_tc a b c f =
  forall    a @@ fun a' ->
  forall    b @@ fun b' ->
  forall_tc c @@ fun c' ->
  f a' b' c'

let forall2_tc a b f =
  forall    a @@ fun a' ->
  forall_tc b @@ fun b' ->
  f a' b'

let (=>) tc ty = (tc , ty)
let (-->) arg ret = p_constant C_arrow     [arg; ret]
let option t      = p_constant C_option    [t]
let pair a b      = p_row_ez   C_record    [("0",a);("1",b)]
let sum  a b      = p_row_ez   C_variant   [("0",a);("1",b)]
(* TODO: this is a placeholder, we need some row variables to allow constraints on all the fields of a record https://gitlab.com/ligolang/ligo/-/merge_requests/1189 *)
let record        = p_row_ez   C_record    []
let variant       = p_row_ez   C_variant   []
let map  k v      = p_constant C_map       [k; v]
let big_map k v   = p_constant C_big_map   [k; v]
let unit          = p_constant C_unit      []
let list   t      = p_constant C_list      [t]
let set    t      = p_constant C_set       [t]
let bool          = p_row_ez C_variant    [("True",unit);("False",unit)]
let string        = p_constant C_string    []
let nat           = p_constant C_nat       []
let mutez         = p_constant C_mutez     []
let timestamp     = p_constant C_timestamp []
let int           = p_constant C_int       []
let address       = p_constant C_address   []
let chain_id      = p_constant C_chain_id  []
let bytes         = p_constant C_bytes     []
let key           = p_constant C_key       []
let key_hash      = p_constant C_key_hash  []
let signature     = p_constant C_signature []
let operation     = p_constant C_operation []
let contract t    = p_constant C_contract  [t]
let bls12_381_g1  = p_constant C_bls12_381_g1 []
let bls12_381_g2  = p_constant C_bls12_381_g2 []
let bls12_381_fr  = p_constant C_bls12_381_fr []
let never         = p_constant C_never      []
let ( * ) a b = pair a b

(* type value of recursive types *)
let comparable = Var.of_name "comparable"
let tc_comparable = 
  let a =Var.fresh () in
   P_abs {arg = a; ret =
    let x = Var.fresh () in
    let y = Var.fresh () in
    Location.wrap @@ P_constraint { pc = { c = C_typeclass {
      tc_bound = [x;y] ;
      tc_constraints =[c_apply comparable x "tc_comparable:bound"; 
                      c_apply comparable y "tc_comparable:bound"];
      tc_args = [p_var a];
      original_id = None;
      typeclass  =  [ 
                [address] ;
                [bool] ;
                [bytes] ;
                [chain_id] ;
                [int] ; 
                [key] ; 
                [key_hash] ;
                [mutez] ; 
                [nat] ; 
                [never] ;
                [option (p_var x)] ;
                [variant] ;
                [record] ;
                [set (p_var x)] ;
                [signature] ;
                [string] ;
                [timestamp] ;
                [unit] ; 
                (* pair of comparable *)
              ]
    }; reason = "default tc"}
  }
}

let storable = Var.of_name "storable" 

let tc_storable =
  let a = Var.fresh () in
   P_abs {arg = a; ret =
    let c = Var.fresh () in
    let x = Var.fresh () in
    let y = Var.fresh () in
    Location.wrap @@ P_constraint { pc = { c = C_typeclass {
      tc_bound = [c;x;y];
      tc_constraints =[
                                 c_apply comparable c "tc_storable:comparable bound";
                                 c_apply storable c "tc_storable:bound";
                                 c_apply storable x "tc_storable:bound"; 
                                 c_apply storable y "tc_storable:bound"];
      tc_args = [p_var a];
      original_id = None;
      typeclass = [ 
                [address] ;
                [big_map (p_var x) (p_var y)] ;
                [bls12_381_fr] ;
                [bls12_381_g1] ;
                [bls12_381_g2] ;
                [bool] ;
                [bytes] ;
                [chain_id] ;
                [int] ; 
                [key] ; 
                [key_hash] ; 
                [(p_var x) --> (p_var y)] ;
                [list (p_var x)] ;
                [map (p_var c) (p_var y)] ;
                [mutez] ; 
                [nat] ; 
                [never] ;
                [option (p_var x)] ;
                [variant] ;
                [record] ;
                (* [sapling_state (var x)] ;
                [sapling_transaction (var x)] ; *)
                [set (p_var c)] ;
                [signature] ;
                [string] ;
                (* [ticket (var x)] ; *)
                [timestamp] ;
                [unit] ; 
              ]
      };reason = ""
    }
  }
}

let packable = Var.of_name "packable"

let tc_packable =  
  let a = Var.fresh () in
   P_abs {arg = a; ret =
    let c = Var.fresh () in
    let x = Var.fresh () in
    let y = Var.fresh () in
    Location.wrap @@ P_constraint { pc = { c = C_typeclass {
      tc_bound = [c;x;y];
      tc_constraints = [
                                      c_apply comparable c "tc_packable: comparable bound";
                                      c_apply packable c "tc_packable:bound";
                                      c_apply packable x "tc_packable:bound"; 
                                      c_apply packable y "tc_packable:bound"];
      tc_args = [p_var a];
      original_id = None;
      typeclass = [ 
                [address] ;
                [bls12_381_fr] ;
                [bls12_381_g1] ;
                [bls12_381_g2] ;
                [bool] ;
                [bytes] ;
                [chain_id] ;
                [contract (p_var x)] ;
                [int] ; 
                [key] ; 
                [key_hash] ; 
                [(p_var x) --> (p_var y)] ;
                [list (p_var x)] ;
                [map (p_var c) (p_var y)] ;
                [mutez] ; 
                [nat] ; 
                [never] ;
                [option (p_var x)] ;
                [variant] ;
                [record] ;
                [set (p_var c)] ;
                [signature] ;
                [string] ;
                [timestamp] ;
                [unit] ; 
              ]
      };
    reason = ""
    }
  }
}



(* These are used temporarily to de-curry functions that correspond to Michelson operators *)
let tuple0        = p_row_ez   C_record    []
let tuple1 a      = p_row_ez   C_record    [("0",a)]
let tuple2 a b    = p_row_ez   C_record    [("0",a);("1",b)]
let tuple3 a b c  = p_row_ez   C_record    [("0",a);("1",b);("2",c)]
