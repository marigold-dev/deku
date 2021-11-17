open Ast_core.Types
open Solver_types
open Ast_core.Combinators

open Db_index_tests_common

module Typeclasses_constraining_tests = struct
  include Test_vars
  module Plugin_under_test = Database_plugins.All_plugins.Typeclasses_constraining
  include Plugin_under_test
  let repr : type_variable -> type_variable = fun tv ->
    match tv with
    | tv when Var.equal tv tva -> tva
    | tv when Var.equal tv tvb -> tva
    | _ -> tv
  let same_state2 ~raise sa sb =
    (* This index is not implemented yet, its state is just a unit value *)
    let sa = UnionFind.ReprMap.bindings @@ (get_state_for_tests sa) in
    let () = tst_assert ~raise "Length saf = Length sbf" (List.length sa = List.length sb) in
    List.iter2_exn
      ~f:(fun a b ->
        let () = tst_assert ~raise ("type variable =" ^ Var.to_name (fst a) ^ " " ^ Var.to_name (fst b)) (Ast_core.Compare.type_variable (fst a) (fst b) = 0) in
        let () = tst_assert ~raise "c_typeclass_simpl set =" (List.compare Ast_core.Compare.c_typeclass_simpl (MultiSet.elements (snd a)) (MultiSet.elements (snd b)) = 0) in
        ()
      )
      sa (List.sort ~compare:(fun (x,_) (y,_) -> Var.compare x y) sb)
  let same_state ~raise sa sb =
    let sb = UnionFind.ReprMap.bindings @@ (get_state_for_tests sb) in
    same_state2 ~raise sa sb
end

let tval ?(loc = Location.generated) tag args = 
  {
    Location.
    wrap_content = P_constant { p_ctor_tag = tag ; p_ctor_args = args; };
    location = loc
  }

let tval_int = tval C_int []
let tval_unit = tval C_unit []
let tval_map_int_unit = tval C_map [tval C_int []; tval C_unit []]

let typeclasses_constraining ~raise () =
  Printf.printf "0000000000000000000";
  let open Typeclasses_constraining_tests in
  let set l = MultiSet.add_list l @@ MultiSet.create ~cmp:Ast_core.Compare.c_typeclass_simpl in
  (* create empty state *)
  let state = create_state ~cmp:Ast_core.Compare.type_variable in
  (* assert state = {} *)
  let () = same_state2 ~raise state [] in

  (* (\* add `tva = unit()' to the state *\)
   * let ctor_a = make_c_constructor_simpl tva C_unit [] in
   * let state' = add_constraint repr state (SC_Constructor ctor_a) in
   * (\* assert state' = {} because only typeclass constraints are indexed by this indexer. *\)
   * let () = same_state2 state' [] in *)
  let state' = state in
  
  (* add ([tvb;tvc] ∈ { [int;unit] , [unit;int] , [map(int,unit);map(int,unit)] } ) to the state *)
  let tc_allowed_bc : type_value list list = [
    [ tval_int ; tval_unit ] ;
    [ tval_unit ; tval_int ] ;
    [ tval_map_int_unit; tval_map_int_unit ] ;
  ] in
  let tc_bc = make_c_typeclass_simpl ~bound:[] ~constraints:[] () 1 None [tvb;tvc] tc_allowed_bc in
  let state'' = add_constraint repr state' (SC_Typeclass tc_bc) in
  Format.printf "%a" (pp Var.pp) state'';
  (* assert state'' = [], [] because there is no refined typeclass yet *)
  let () = same_state2 ~raise state'' [
      (tva, set[tc_bc]);
      (tvc, set[tc_bc]);
    ]
  in

  (* add ([tvb] ∈ { [int] , [unit] }) as a refinement of tc_bc to the state *)
  let tc_allowed_b : type_value list list = [
    [ tval_int ] ;
    [ tval_unit ] ;
  ] in
  let tc_b = make_c_typeclass_simpl ~bound:[] ~constraints:[] ()  2 (Some 1) [tvb] tc_allowed_b in
  let state''' = add_constraint repr state'' (SC_Typeclass tc_b) in
  (* assert state''' = … *)
  let () = same_state2 ~raise state''' [
      (tva, set[tc_bc;tc_b]);  (* TODO: should the old one still be there? *)
      (tvc, set[tc_bc]);        (* TODO: should the old one still be there? *)
    ]
  in

  (* merging tva to tvd *)
  let merge_keys  : (type_variable, type_variable) merge_keys =
    let demoted_repr = tva in
    let new_repr = tvd in
    {
      map = (fun m -> UnionFind.ReprMap.alias ~demoted_repr ~new_repr m);
      set = (fun s -> UnionFind.ReprSet.alias ~demoted_repr ~new_repr s);
    }
  in
  let state'''' = merge_aliases merge_keys state''' in
  (* assert that c has been merged to a in state'''' *)
  (* state'''' = same as above, because this indexer does not store any type variable. *)
  let () = same_state2 ~raise state'''' [
      (tvd, set[tc_bc;tc_b]);  (* TODO: should the old one still be there? *)
      (tvc, set[tc_bc]);        (* TODO: should the old one still be there? *)
    ] in
  ()
  
