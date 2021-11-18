open Trace

(* module Core = Typesystem.Core *)
open Ast_core.Types
open Solver_types
(* open Ast_core.Reasons *)
open Ast_core.Combinators

open Db_index_tests_common

module Assignments_tests = struct
  include Test_vars
  module Plugin_under_test = Database_plugins.All_plugins.Assignments
  include Plugin_under_test
  let repr : type_variable -> type_variable = fun tv ->
    match tv with
    | tv when Var.equal tv tva -> tva
    | tv when Var.equal tv tvb -> tva
    | _ -> tv
  let same_state ~raise sa sb =
    let sa = bindings sa in
    let sb = bindings sb in
    let () = tst_assert ~raise "Length sa = Length sb" (List.length sa = List.length sb) in
    List.iter ~f:
      (fun ((tva,cora) , (tvb,corb)) ->
         let () = tst_assert ~raise "" (Ast_core.Compare.type_variable tva tvb = 0) in
         let () = tst_assert ~raise "" (Ast_core.Compare.constructor_or_row cora corb = 0) in
         ()
      )
      (List.zip_exn sa sb)
end

open Assignments_tests

(* Test independant add + remove + merge pour chaque type de contraintes:
   test add ctor constraint + add other ctor constraint + merge ctor constraint + add third ctor constraint
*)
let ctor_add_and_merge ~raise () =
  (* create empty state *)
  let state = create_state ~cmp:Ast_core.Compare.type_variable in
  (* assert state = [] *)
  let () = tst_assert ~raise "length(bindings) = 0" @@ (List.length (bindings state) = 0) in

  (* add (tva, SC_Constructor ctor_a) to the state *)
  let ctor_a = make_c_constructor_simpl 1 None tva C_unit [] in
  let state' = add_constraint ~debug:Ast_core.PP.type_variable repr state (SC_Constructor ctor_a) in                                           
  (* assert state = [ (tva , `Constructor ctor_a) ] *)
  let () =
    match bindings state' with
    | [(tv,cor)] ->
      let () = tst_assert ~raise "state' : cor = ctor" @@ (Ast_core.Compare.constructor_or_row cor (`Constructor ctor_a) = 0) in
      let () = tst_assert ~raise "state' : tv = tva" @@ Var.equal tv tva in
      ()
    | x -> raise.raise (test_err (Format.asprintf "state should only have one elment but has %d elements" @@ List.length x))
  in

  (* add (tvb, SC_Constructor ctor_b) to the state (tvb being an alias of tva, see repr) *)
  let ctor_b = make_c_constructor_simpl 2 None tvb C_unit [] in
  let state'' = add_constraint ~debug:Ast_core.PP.type_variable repr state' (SC_Constructor ctor_b) in
  (* assert that state did not update because a and b are aliases*)
  let () = tst_assert ~raise "state'' = state'" @@ (List.length (bindings state'') = 1) in

  (* add (tvc, SC_Constructor ctor_c) *)
  let ctor_c = make_c_constructor_simpl 3 None tvc C_unit [] in
  let state''' = add_constraint ~debug:Ast_core.PP.type_variable repr state'' (SC_Constructor ctor_c) in
  (* assert that state''' now has two elements *)
  let () = tst_assert ~raise "length (state''') = 2" (List.length (bindings state''') = 2) in

  (* merging tvc to tva *)
  let merge_keys  : (type_variable, type_variable) merge_keys =
    let demoted_repr = tvc in
    let new_repr = tva in
    {
      map = (fun m -> UnionFind.ReprMap.alias  ~debug:(fun ppf (a,_) -> Ast_core.PP.type_variable ppf a) ~demoted_repr ~new_repr m);
      set = (fun s -> UnionFind.ReprSet.alias ~demoted_repr ~new_repr s);
    }
  in
  let state'''' = merge_aliases merge_keys state''' in
  (* assert that c has been merged to a in state'''' *)
  let () =
    match bindings state'''' with
    | [(tv,cor)] ->
      let () = tst_assert ~raise "state'''' : cor = ctor" @@ (Ast_core.Compare.constructor_or_row cor (`Constructor ctor_a) = 0) in
      let () = tst_assert ~raise "state'''' : tv = tva" @@ Var.equal tv tva in
      ()
    | l -> Format.printf "%a" (PP_helpers.list_sep_d (PP_helpers.pair Ast_core.PP.type_variable Ast_core.PP.constructor_or_row)) l
  in 
  ()

(*
   test add ctor constraint + add other ctor constraint + remove ctor constraint + add third ctor constraint

   assignment cannot be removed
*)
   
(*
   test add row constraint + add other row constraint + merge row constraint + add third row constraint
*)
   
let row_add_and_merge ~raise () =
  (* create empty state *)
  let state = create_state ~cmp:Ast_core.Compare.type_variable in
  (* assert state = [] *)
  let () = tst_assert ~raise "length(bindings) = 0" @@ (List.length (bindings state) = 0) in

  (* add (tva, SC_Constructor ctor_a) to the state *)
  let row_a = make_c_row_simpl 4 None tva C_record [] in
  let state' = add_constraint repr state (SC_Row row_a) in                                           
  (* assert state = [ (tva , `Constructor ctor_a) ] *)
  let () =
    match bindings state' with
    | [(tv,cor)] ->
      let () = tst_assert ~raise "state' : cor = ctor" @@ (Ast_core.Compare.constructor_or_row cor (`Row row_a) = 0) in
      let () = tst_assert ~raise "state' : tv = tva" @@ Var.equal tv tva in
      ()
    | x -> raise.raise (test_err (Format.asprintf "state should only have one elment but has %d elements" @@ List.length x))
  in

  (* add (tvb, SC_Constructor ctor_b) to the state (tvb being an alias of tva, see repr) *)
  let row_b = make_c_row_simpl 5 None tvb C_record [] in
  let state'' = add_constraint repr state' (SC_Row row_b) in
  (* assert that state did not update because a and b are aliases*)
  let () = tst_assert ~raise "state'' = state'" @@ (List.length (bindings state'') = 1) in

  (* add (tvc, SC_Constructor ctor_c) *)
  let row_c = make_c_row_simpl 6 None tvc C_record [] in
  let state''' = add_constraint repr state'' (SC_Row row_c) in
  (* assert that state''' now has two elements *)
  let () = tst_assert ~raise "length (state''') = 2" (List.length (bindings state''') = 2) in

  (* merging tvc to tva *)
  let merge_keys  : (type_variable, type_variable) merge_keys =
    let demoted_repr = tvc in
    let new_repr = tva in
    {
      map = (fun m -> UnionFind.ReprMap.alias ~demoted_repr ~new_repr m);
      set = (fun s -> UnionFind.ReprSet.alias ~demoted_repr ~new_repr s);
    }
  in
  let state'''' = merge_aliases merge_keys state''' in
  (* assert that c has been merged to a in state'''' *)
  let () =
    match bindings state'''' with
    | [(tv,cor)] ->
      let () = tst_assert ~raise "state'''' : cor = ctor" @@ (Ast_core.Compare.constructor_or_row cor (`Row row_a) = 0) in
      let () = tst_assert ~raise "state'''' : tv = tva" @@ Var.equal tv tva in
      ()
    | l -> Format.printf "%a" (PP_helpers.list_sep_d (PP_helpers.pair Ast_core.PP.type_variable Ast_core.PP.constructor_or_row)) l
  in 
  ()

(*
  test add row constraint + add other row constraint + remove row constraint + add third row constraint
*)

(*
   test mixed

   *)

(* Test that the order of add and merge doesn't matter *)

type test_seq = Add_cstr of type_variable | Merge of (type_variable , type_variable) merge_keys
let invariant ~raise () =
  let repr : type_variable -> type_variable = fun tv -> tv in
  let merge_keys demoted_repr new_repr  : (type_variable, type_variable) merge_keys =
    {
      map = (fun m -> UnionFind.ReprMap.alias ~debug:(fun ppf (a,_) -> Ast_core.PP.type_variable ppf a) ~demoted_repr ~new_repr m);
      set = (fun s -> UnionFind.ReprSet.alias ~demoted_repr ~new_repr s);
    }
  in

  (* create empty state *)
  let istate = create_state ~cmp:Ast_core.Compare.type_variable in
  let aux : _ t -> test_seq -> _ t = fun state seq ->
    match seq with
    | Add_cstr tv ->
      let tc = SC_Constructor (make_c_constructor_simpl 7 None tv C_unit []) in
      add_constraint ~debug:Ast_core.PP.type_variable repr state tc
    | Merge merge_keys -> merge_aliases merge_keys state
  in
  let state_a = List.fold_left ~f:aux ~init:istate
    [ Add_cstr tva ; Add_cstr tvb ; Add_cstr tvc ; Merge (merge_keys tva tvb) ; ]
  in
  let state_b = List.fold_left ~f:aux ~init:istate
    [ Add_cstr tva ; Add_cstr tvb ; Merge (merge_keys tva tvb) ; Add_cstr tvc ; ]
  in
  let () = same_state ~raise state_a state_b in
  ()
  
let assignments ~raise () =
  let () = ctor_add_and_merge ~raise () in
  let () = row_add_and_merge ~raise () in
  let () = invariant ~raise () in
  ()
