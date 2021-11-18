open Trace
open Ast_core.Types
open Solver_types
open Database_plugins.All_plugins
open Db_index_tests_common

(* can't be defined easily in MultiSet.ml because it doesn't have access to List.compare ~cmp  *)
let multiset_compare a b =
  let ab = List.compare (MultiSet.get_compare a) (MultiSet.elements a) (MultiSet.elements b) in
  let ba = List.compare (MultiSet.get_compare b) (MultiSet.elements a) (MultiSet.elements b) in
  if ab != ba
  then failwith "Internal error: bad test: sets being compared have different comparison functions!"
  else ab

module Grouped_by_variable_tests = struct
  include Test_vars
  module Plugin_under_test = Database_plugins.All_plugins.Grouped_by_variable
  include Plugin_under_test
  let repr : type_variable -> type_variable = fun tv ->
    match tv with
    | tv when Var.equal tv tva -> tva
    | tv when Var.equal tv tvb -> tva
    | _ -> tv

  let cmp x y =
    List.compare (Pair.compare Var.compare multiset_compare)
      (List.filter ~f:(fun (_,s) -> not (MultiSet.is_empty s)) x)
      (List.filter ~f:(fun (_,s) -> not (MultiSet.is_empty s)) y)
  let same_state' ~raise loc (expected : _ t_for_tests) (actual : _ t_for_tests) =
    let expected_actual_str =
      let open PP_helpers in
      let pp' pp x = (list_sep_d (pair Var.pp (MultiSet.pp pp))) x in
      Format.asprintf "expected=\n{ctors=\n%a;\nrows=\n%a;\npolys=\n%a;\naccess_result=\n%a;\naccess_record=\n%a}\nactual=\n{ctors=\n%a;\nrows=\n%a;\npolys=\n%a;\naccess_result=\n%a;\naccess_record=\n%a}"
        (pp' Ast_core.PP.c_constructor_simpl) expected.constructor
        (pp' Ast_core.PP.c_row_simpl        ) expected.row
        (pp' Ast_core.PP.c_poly_simpl       ) expected.poly
        (pp' Ast_core.PP.c_access_label_simpl) expected.access_label_by_result_type
        (pp' Ast_core.PP.c_access_label_simpl) expected.access_label_by_record_type
        (pp' Ast_core.PP.c_constructor_simpl) actual.constructor
        (pp' Ast_core.PP.c_row_simpl        ) actual.row
        (pp' Ast_core.PP.c_poly_simpl       ) actual.poly
        (pp' Ast_core.PP.c_access_label_simpl) actual.access_label_by_result_type
        (pp' Ast_core.PP.c_access_label_simpl) actual.access_label_by_record_type
    in
    let a msg expected actual =
      tst_assert ~raise (Format.asprintf "%s\n%s\n%s\n" msg loc expected_actual_str)
        (cmp expected actual = 0)
    in
    let () = a "lists of ctors must be equal" expected.constructor actual.constructor in
    let () = a "lists of rows must be equal"  expected.row actual.row in
    let () = a "lists of polys must be equal" expected.poly actual.poly in
    ()

  let same_state ~raise (expected : _ t) (actual : _ t) =
    same_state' ~raise __LOC__ (Grouped_by_variable.bindings expected) (Grouped_by_variable.bindings actual)
end

open! Grouped_by_variable_tests

let merge_in_state ~demoted_repr ~new_repr state =
  let updater = {
    map = (fun m -> UnionFind.ReprMap.alias ~demoted_repr ~new_repr m);
    set = (fun s -> UnionFind.ReprSet.alias ~demoted_repr ~new_repr s);
  } in
  merge_aliases updater state

let merge_in_repr ~demoted_repr ~new_repr repr =
  fun tv -> match repr tv with
      tv when Var.equal tv demoted_repr -> new_repr
    | other -> other

let merge ~demoted_repr ~new_repr repr state =
  if (not (Var.equal (repr demoted_repr) demoted_repr)) ||
     (not (Var.equal (repr new_repr) new_repr))
  then
    failwith "Internal error: bad test: the demoted_repr and new_repr \
              should already be representants when merge is called."
  else
    ((merge_in_repr ~demoted_repr ~new_repr repr),
     (merge_in_state ~demoted_repr ~new_repr state))

type nonrec t_for_tests = type_variable Grouped_by_variable.t_for_tests

let filter_only_ctors  = List.filter_map ~f:(function Ast_core.Types.SC_Constructor c -> Some c | _ -> None)
let filter_only_rows   = List.filter_map ~f:(function Ast_core.Types.SC_Row         c -> Some c | _ -> None)
let filter_only_polys  = List.filter_map ~f:(function Ast_core.Types.SC_Poly        c -> Some c | _ -> None)
let filter_only_access_labels = List.filter_map ~f:(function Ast_core.Types.SC_Access_label c -> Some c | _ -> None)
let to_ctor_sets = List.map ~f:(fun (v,cs) -> (v, (MultiSet.of_list ~cmp:Ast_core.Compare.c_constructor_simpl (filter_only_ctors cs))))
let to_row_sets  = List.map ~f:(fun (v,cs) -> (v, (MultiSet.of_list ~cmp:Ast_core.Compare.c_row_simpl         (filter_only_rows  cs))))
let to_poly_sets = List.map ~f:(fun (v,cs) -> (v, (MultiSet.of_list ~cmp:Ast_core.Compare.c_poly_simpl        (filter_only_polys cs))))
let to_access_label_sets = List.map ~f:(fun (v,cs) -> (v, (MultiSet.of_list ~cmp:Ast_core.Compare.c_access_label_simpl (filter_only_access_labels cs))))

let assert_states_equal ~raise
    loc
    ?(expected_ctors:(type_variable * type_constraint_simpl list) list = [])
    ?(expected_rows:(type_variable * type_constraint_simpl list) list = [])
    ?(expected_polys:(type_variable * type_constraint_simpl list) list = [])
    ?(expected_access_labels_by_result_type:(type_variable * type_constraint_simpl list) list = [])
    ?(expected_access_labels_by_record_type:(type_variable * type_constraint_simpl list) list = [])
    (actual:type_variable t) =
  same_state' ~raise
    loc
    {
      constructor = to_ctor_sets expected_ctors ;
      row         = to_row_sets  expected_rows  ;
      poly        = to_poly_sets expected_polys ;
      access_label_by_result_type = to_access_label_sets expected_access_labels_by_result_type ;
      access_label_by_record_type = to_access_label_sets expected_access_labels_by_record_type ;
    }
    (Grouped_by_variable.bindings actual)

let remove_constraint ~raise repr state constraint_to_rm =
  trace ~raise Main_errors.inference_tracer @@
  remove_constraint Ast_core.PP.type_variable repr state constraint_to_rm

let first_test ~raise () =
  let repr : type_variable -> type_variable = fun tv -> tv in
  let state = create_state ~cmp:Ast_core.Compare.type_variable in

  let repr = merge_in_repr ~demoted_repr:tvb ~new_repr:tva repr in

  (* create constraints and add them to the state *)
  let sc_a : type_constraint_simpl = constructor 1 None tva C_unit [] in
  let sc_b : type_constraint_simpl = constructor 2 None tvb C_unit [] in
  let sc_c : type_constraint_simpl = constructor 3 None tvc C_unit [] in
  let state = add_constraint repr state sc_a in
  let state = add_constraint repr state sc_b in
  let state = add_constraint repr state sc_c in
  (* 
    check that :
    - a is associated with sc_a and sc_b
    - c is associated wit sc_c
    - b has no associated constraint (because repr(b) = a)
  *)
  let () = assert_states_equal ~raise __LOC__
    ~expected_ctors:[(tva, [sc_a ; sc_b]) ; (tvc, [sc_c])]
    state in

  ()

let second_test ~raise () =
  let repr : type_variable -> type_variable = fun tv -> tv in
  let state = create_state ~cmp:Ast_core.Compare.type_variable in

  let repr = merge_in_repr ~demoted_repr:tvb ~new_repr:tva repr in

  (* create constraints and add them to the state *)
  let sc_a : type_constraint_simpl = constructor 1 None tva C_unit [] in
  let sc_b : type_constraint_simpl = constructor 2 None tvb C_unit [] in
  let sc_c : type_constraint_simpl = constructor 3 None tvc C_unit [] in
  let state = add_constraint repr state sc_a in
  let state = add_constraint repr state sc_b in
  let state = add_constraint repr state sc_c in
  (* 
    check that :
    - a is associated with sc_a and sc_b
    - c is associated wit sc_c
    - b has no associated constraint (because repr(b) = a)
  *)
  let () = assert_states_equal ~raise __LOC__
      ~expected_ctors:[(tva, [sc_a ; sc_b]) ; (tvc, [sc_c])]
      state in

  (* remove sc_a from state *)
  let state = remove_constraint ~raise repr state sc_a in
  (* same check as above except sc_a should be deleted from tva's constraints *)
  let () = assert_states_equal ~raise __LOC__
      ~expected_ctors:[(tva, [sc_b]) ; (tvc, [sc_c])]
      state in

  (* merge variable c into a *)
  let repr, state = merge ~demoted_repr:tvc ~new_repr:tva repr state in
  (* same check as above except sc_c should now be in a's constraints *)
  let () = assert_states_equal ~raise __LOC__
      ~expected_ctors:[(tva, [sc_b; sc_c])]
      state in

  (* create constraint and add it to the state *)
  let sc_d : type_constraint_simpl = constructor 4 None tvd C_unit [] in
  let state = add_constraint repr state sc_d in
  (* same check as above except sc_d should be added to d's constraints (was empty / absent before) *)
  let () = assert_states_equal ~raise __LOC__
      ~expected_ctors:[(tva, [sc_b; sc_c]) ; (tvd, [sc_d])]
      state in

  (* create constraint and add it to the state *)
  let sc_a2 : type_constraint_simpl = constructor 5 None tva C_unit [] in
  let state = add_constraint repr state sc_a2 in
  (* same check as above except sc_d should be added to a's constraints *)
  let () = assert_states_equal ~raise __LOC__
      ~expected_ctors:[(tva, [sc_a2; sc_b; sc_c]) ; (tvd, [sc_d])]
      state in

  (* create constraint and add it to the state *)
  let sc_b2 : type_constraint_simpl = constructor 6 None tvb C_unit [] in
  let state = add_constraint repr state sc_b2 in
  (* same check as above except sc_d should be added to a's constraints *)
  let () = assert_states_equal ~raise __LOC__
      ~expected_ctors:[(tva, [sc_a2; sc_b; sc_b2; sc_c]) ; (tvd, [sc_d])]
      state in

  ()

let add_and_merge ~raise ~sc_a ~sc_b ~sc_c check =
  let repr : type_variable -> type_variable = fun tv -> tv in
  let state = create_state ~cmp:Ast_core.Compare.type_variable in

  (* Add contraint sc_a *)
  let state = add_constraint repr state sc_a in

  (* Test one; state is { a -> [sc_a]} *)
  let () = check ~raise __LOC__ [(tva, [sc_a])] state in

  (* Add constraint sc_b *)
  let state = add_constraint repr state sc_b in

  (* Test two; state is { a -> [sc_a]; b -> [sc_b]} *)
  let () = check ~raise __LOC__ [(tva, [sc_a]); (tvb, [sc_b])] state in

  (* merge variable b into a *)
  let repr, state = merge ~demoted_repr:tvb ~new_repr:tva repr state in
  
  (* Test three; state is { a -> [sc_a;sc_b]} *)
  (* same check as above except sc_b should now be in a's constraints *)
  let () = check ~raise __LOC__ [(tva, [sc_a; sc_b])] state in

  (* Add constraint sc_c *)
  let state = add_constraint repr state sc_c in

  (* Test four; state is { a -> [sc_a;sc_b]; c -> [sc_c]} *)
  let () = check ~raise __LOC__ [(tva, [sc_a; sc_b]) ; (tvc, [sc_c])] state in

  ()

(* test add ctor constraint + add other ctor constraint + merge + add third ctor constraint *)
let ctor_add_and_merge () =
  let sc_a : type_constraint_simpl = constructor 1 None tva C_unit [] in
  let sc_b : type_constraint_simpl = constructor 2 None tvb C_unit [] in
  let sc_c : type_constraint_simpl = constructor 3 None tvc C_unit [] in
  add_and_merge ~sc_a ~sc_b ~sc_c (fun ~raise loc expected_ctors state -> assert_states_equal ~raise (loc ^ "\n" ^ __LOC__) ~expected_ctors state)

(* test add row constraint + add other row constraint + merge + add third row constraint *)
let row_add_and_merge () =
  let sc_a : type_constraint_simpl = row 1 tva in
  let sc_b : type_constraint_simpl = row 2 tvb in
  let sc_c : type_constraint_simpl = row 3 tvc in
  add_and_merge ~sc_a ~sc_b ~sc_c (fun ~raise  loc expected_rows state -> assert_states_equal ~raise (loc ^ "\n" ^ __LOC__) ~expected_rows state)


(* test add poly constraint + add other poly constraint + merge poly constraint + add third poly constraint *)
let poly_add_and_merge () =
  let p_forall : p_forall = {
    binder = Var.of_name "binder";
    constraints = [];
    body = Location.wrap @@ P_variable (Var.of_name "binder");
  } in
  let sc_a : type_constraint_simpl = poly tva p_forall in
  let sc_b : type_constraint_simpl = poly tvb p_forall in
  let sc_c : type_constraint_simpl = poly tvc p_forall in
  add_and_merge ~sc_a ~sc_b ~sc_c (fun ~raise  loc expected_polys state -> assert_states_equal ~raise (loc ^ "\n" ^ __LOC__) ~expected_polys state)

let access_label_add_and_merge ~raise  () =
  let sc_abf : type_constraint_simpl = access_label tva ~record_type:tvb labelfoo in
  let sc_bbb : type_constraint_simpl = access_label tvb ~record_type:tvb labelbar in
  let sc_ccb : type_constraint_simpl = access_label tvc ~record_type:tvc labelbar in
  let check = (fun ~raise  loc expected_access_labels_by_result_type expected_access_labels_by_record_type state ->
      assert_states_equal ~raise (loc ^ "\n" ^ __LOC__) ~expected_access_labels_by_result_type ~expected_access_labels_by_record_type state) in

  let repr : type_variable -> type_variable = fun tv -> tv in
  let state = create_state ~cmp:Ast_core.Compare.type_variable in

  let state = add_constraint repr state sc_abf in
  let () = check ~raise  __LOC__ [(tva, [sc_abf])] [(tvb, [sc_abf])] state in
  let state = add_constraint repr state sc_bbb in
  let () = check ~raise  __LOC__ [(tva, [sc_abf]); (tvb, [sc_bbb])] [(tvb, [sc_abf; sc_bbb])] state in
  let repr, state = merge ~demoted_repr:tvb ~new_repr:tva repr state in
  let () = check ~raise  __LOC__ [(tva, [sc_abf; sc_bbb])] [(tva, [sc_abf; sc_bbb])] state in
  let state = add_constraint repr state sc_ccb in
  let () = check ~raise  __LOC__ [(tva, [sc_abf; sc_bbb]) ; (tvc, [sc_ccb])] [(tva, [sc_abf; sc_bbb]); (tvc, [sc_ccb])] state in

  ()



let add_and_remove ~raise ~sc_a ~sc_b ~sc_c check =
  let repr : type_variable -> type_variable = fun tv -> tv in
  let state = create_state ~cmp:Ast_core.Compare.type_variable in

  (* Add contraint sc_a *)
  let state = add_constraint repr state sc_a in

  (* Test one; state is { a -> [sc_a]} *)
  let () = check ~raise __LOC__ [(tva, [sc_a])] state in
  
  (* Add constraint sc_b *)
  let state = add_constraint repr state sc_b in

  (* Test two; state is { a -> [sc_a]; b -> [sc_b]} *)
  let () = check ~raise __LOC__ [(tva, [sc_a]); (tvb, [sc_b])] state in

  (* Remove constaint sc_b *)
  let state = remove_constraint ~raise repr state sc_b in
  (* Test three; state is { a -> [sc_a]} *)
  let () = check ~raise __LOC__ [(tva, [sc_a])] state in

  (* Add constraint sc_c *)
  let state = add_constraint repr state sc_c in

  (* Test four; state is { a -> [sc_a]; c -> [sc_b]} *)
  let () = check ~raise __LOC__ [(tva, [sc_a]) ; (tvc, [sc_c])] state in

  ()

(*
    test add ctor constraint + add other ctor constraint + remove ctor constraint + add third ctor constraint
*)
let ctor_add_and_remove () =
  let sc_a : type_constraint_simpl = constructor 1 None tva C_unit [] in
  let sc_b : type_constraint_simpl = constructor 2 None tvb C_unit [] in
  let sc_c : type_constraint_simpl = constructor 3 None tvc C_unit [] in
  add_and_remove ~sc_a ~sc_b ~sc_c (fun ~raise loc expected_ctors state -> assert_states_equal ~raise (loc ^ "\n" ^ __LOC__) ~expected_ctors state)

(*
   test add row constraint + add other row constraint + remove row constraint + add third row constraint
*)
let row_add_and_remove () =
  let sc_a : type_constraint_simpl = row 1 tva in
  let sc_b : type_constraint_simpl = row 2 tvb in
  let sc_c : type_constraint_simpl = row 3 tvc in
  add_and_remove ~sc_a ~sc_b ~sc_c (fun ~raise loc expected_rows state -> assert_states_equal ~raise (loc ^ "\n" ^ __LOC__) ~expected_rows state)


(*
   test add poly constraint + add other poly constraint + remove poly constraint + add third poly constraint
*)
let poly_add_and_remove () =
  let p_forall : p_forall = {
    binder = Var.of_name "binder";
    constraints = [];
    body = Location.wrap @@ P_variable (Var.of_name "binder");
  } in
  let sc_a : type_constraint_simpl = poly tva p_forall in
  let sc_b : type_constraint_simpl = poly tvb p_forall in
  let sc_c : type_constraint_simpl = poly tvc p_forall in
  add_and_remove ~sc_a ~sc_b ~sc_c (fun ~raise loc expected_polys state -> assert_states_equal ~raise (loc ^ "\n" ^ __LOC__) ~expected_polys state)

let access_label_add_and_remove ~raise () =
  let sc_baf : type_constraint_simpl = access_label tvb ~record_type:tva labelfoo in
  let sc_bbb : type_constraint_simpl = access_label tvb ~record_type:tvb labelbar in
  let sc_ccb : type_constraint_simpl = access_label tvc ~record_type:tvc labelbar in
  let check = (fun ~raise loc expected_access_labels_by_result_type expected_access_labels_by_record_type state ->
      assert_states_equal ~raise (loc ^ "\n" ^ __LOC__) ~expected_access_labels_by_result_type ~expected_access_labels_by_record_type state) in

  let repr : type_variable -> type_variable = fun tv -> tv in
  let state = create_state ~cmp:Ast_core.Compare.type_variable in

  let state = add_constraint repr state sc_baf in
  let () = check ~raise __LOC__ [(tvb, [sc_baf])] [(tva, [sc_baf])] state in
  let state = add_constraint repr state sc_bbb in
  let () = check ~raise __LOC__ [tvb, [sc_baf; sc_bbb]] [(tva, [sc_baf]); (tvb, [sc_bbb])] state in
  let state = remove_constraint ~raise repr state sc_bbb in
  let () = check ~raise __LOC__ [(tvb, [sc_baf])] [(tva, [sc_baf])] state in
  let state = add_constraint repr state sc_ccb in
  let () = check ~raise __LOC__ [(tvb, [sc_baf]); (tvc, [sc_ccb])] [(tva, [sc_baf]) ; (tvc, [sc_ccb])] state in

  ()

(* Test mixed + remove + merge *)

let mixed ~raise () =
  Format.printf "In mixed \n%!";
  let repr : type_variable -> type_variable = fun tv -> tv in
  let state = create_state ~cmp:Ast_core.Compare.type_variable in

  (* add ctor *)
  let sc_a : type_constraint_simpl = constructor 10 None tva C_unit [] in
  let state = add_constraint repr state sc_a in

  (* Test 1: state is { a -> [sc_a]} *)
  Format.printf "Test 1\n%!";
  let () = assert_states_equal ~raise __LOC__
      ~expected_ctors:[(tva, [sc_a])]
      state in
  
  (* Add row *)
  let sc_b : type_constraint_simpl = row 11 tvb in
  let state = add_constraint repr state sc_b in
  (* Test 2: state is { a -> [sc_a]; b -> [sc_b]} *)

  (* Test 2; state is ctors = {a -> [sc_a]} rows = {b -> [sc_b]} *)
  Format.printf "Test 2\n%!";
  let () = assert_states_equal ~raise __LOC__
      ~expected_ctors:[(tva, [sc_a])]
      ~expected_rows:[(tvb, [sc_b])]
      state in

  (* Add poly*)
  let p_forall : p_forall = {
    binder = Var.of_name "binder";
    constraints = [];
    body = Location.wrap @@ P_variable (Var.of_name "binder");
  } in
  let sc_c : type_constraint_simpl = poly tvc p_forall in
  let state = add_constraint repr state sc_c in

  (* Test 3; state is ctors = {a -> [sc_a]} rows = {b -> [sc_b]} polys = {c -> [sc_c]} *)
  Format.printf "Test 3\n%!";
  let () = assert_states_equal ~raise __LOC__
      ~expected_ctors:[(tva, [sc_a])]
      ~expected_rows:[(tvb, [sc_b])]
      ~expected_polys:[(tvc, [sc_c])]
      state in

  (* Add constraint sc_c2 *)
  let sc_c2 = constructor 12 None tvc C_int [] in
  let state = add_constraint repr state sc_c2 in
  (* Test 4; state is ctors = {a -> [sc_a]; c -> [sc_c2]} rows = {b -> [sc_b]} polys = {c -> [sc_c]} *)
  let () = assert_states_equal ~raise __LOC__
      ~expected_ctors:[(tva, [sc_a]) ; (tvc, [sc_c2])]
      ~expected_rows:[(tvb, [sc_b])]
      ~expected_polys:[(tvc, [sc_c])]
      state in
  
  (* merge variable b into a *)
  let repr, state = merge ~demoted_repr:tvb ~new_repr:tva repr state in

  (* Test 5; state is ctors = {a -> [sc_a]; c -> [sc_c2]} rows = {a -> [sc_b]} polys = {c -> [sc_c]} *)
  Format.printf "Test 5\n%!";
  let () = assert_states_equal ~raise __LOC__
      ~expected_ctors:[(tva, [sc_a]); (tvc, [sc_c2])]
      ~expected_rows:[(tva, [sc_b])]
      ~expected_polys:[(tvc, [sc_c])]
      state in

  (* Add constraint sc_b2 *)
  let sc_b2 = row ~row:[(Label "foo", tva)] 13 tvb in
  let state = add_constraint repr state sc_b2 in

  (* Test 6; state is ctors = {a -> [sc_a]; c -> [sc_c2]} rows = {a -> [sc_b; sc_b2]} polys = {c -> [sc_c]} *)
  Format.printf "Test 6\n%!";
  let () = assert_states_equal ~raise __LOC__
      ~expected_ctors:[(tva, [sc_a]); (tvc, [sc_c2])]
      ~expected_rows:[(tva, [sc_b; sc_b2])]
      ~expected_polys:[(tvc, [sc_c])]
      state in

  (* Remove constaint sc_b *)
  let state = remove_constraint ~raise repr state sc_b in

  (* Test 7; state is ctors = {a -> [sc_a]; c -> [sc_c2]} rows = {a -> [sc_b2]} polys = {c -> [sc_c]} *)
  Format.printf "Test 7\n%!";
  let () = assert_states_equal ~raise __LOC__
      ~expected_ctors:[(tva, [sc_a]); (tvc, [sc_c2])]
      ~expected_rows:[(tva, [sc_b2])]
      ~expected_polys:[(tvc, [sc_c])]
      state in

  (* Add constraint sc_b3 *)
  let sc_b3 = row ~row:[(Label "foo", tva)] 13 tvb in
  let state = add_constraint repr state sc_b3 in

  (* Test 8; state is ctors = {a -> [sc_a]; c -> [sc_c2]} rows = {a -> [sc_b2; sc_b3]} polys = {c -> [sc_c]} *)
  Format.printf "Test 8\n%!";
  let () = assert_states_equal ~raise __LOC__
      ~expected_ctors:[(tva, [sc_a]); (tvc, [sc_c2])]
      ~expected_rows:[(tva, [sc_b2; sc_b3])]
      ~expected_polys:[(tvc, [sc_c])]
      state in

  (* Remove constaint sc_b2 *)
  let state = remove_constraint ~raise repr state sc_b2 in

  (* Test 9; state is ctors = {a -> [sc_a]; c -> [sc_c2]} rows = {a -> [sc_b3]} polys = {c -> [sc_c]} *)
  Format.printf "Test 9\n%!";
  let () = assert_states_equal ~raise __LOC__
      ~expected_ctors:[(tva, [sc_a]); (tvc, [sc_c2])]
      ~expected_rows:[(tva, [sc_b3])]
      ~expected_polys:[(tvc, [sc_c])]
      state in

  let sc_acf = access_label tva ~record_type:tvc labelfoo in
  let state = add_constraint repr state sc_acf in

  let () = assert_states_equal ~raise __LOC__
      ~expected_ctors:[(tva, [sc_a]); (tvc, [sc_c2])]
      ~expected_rows:[(tva, [sc_b3])]
      ~expected_polys:[(tvc, [sc_c])]
      ~expected_access_labels_by_result_type:[(tva, [sc_acf])]
      ~expected_access_labels_by_record_type:[(tvc, [sc_acf])]
      state in

  let sc_bcb = access_label tvb ~record_type:tvc labelbar in
  let state = add_constraint repr state sc_bcb in

  let () = assert_states_equal ~raise __LOC__
      ~expected_ctors:[(tva, [sc_a]); (tvc, [sc_c2])]
      ~expected_rows:[(tva, [sc_b3])]
      ~expected_polys:[(tvc, [sc_c])]
      ~expected_access_labels_by_result_type:[(tva, [sc_acf; sc_bcb])]
      ~expected_access_labels_by_record_type:[(tvc, [sc_acf; sc_bcb])]
      state in

  let sc_dcb = access_label tvc ~record_type:tvb labelbar in
  let state = add_constraint repr state sc_dcb in

  let () = assert_states_equal ~raise __LOC__
      ~expected_ctors:[(tva, [sc_a]); (tvc, [sc_c2])]
      ~expected_rows:[(tva, [sc_b3])]
      ~expected_polys:[(tvc, [sc_c])]
      ~expected_access_labels_by_result_type:[(tva, [sc_acf; sc_bcb]); (tvc, [sc_dcb])]
      ~expected_access_labels_by_record_type:[(tvc, [sc_acf; sc_bcb]); (tva, [sc_dcb])]
      state in

  Format.printf "Before remove constraint :%a\n"
    Ast_core.PP.type_constraint_simpl_short sc_acf
    ;
  let state = remove_constraint ~raise repr state sc_acf in
  Format.printf "Afer remove\n%!";


  let () = assert_states_equal ~raise __LOC__
      ~expected_ctors:[(tva, [sc_a]); (tvc, [sc_c2])]
      ~expected_rows:[(tva, [sc_b3])]
      ~expected_polys:[(tvc, [sc_c])]
      ~expected_access_labels_by_result_type:[(tva, [sc_bcb]); (tvd, [sc_dcb])]
      ~expected_access_labels_by_record_type:[(tvc, [sc_bcb; sc_dcb])]
      state in

  ()

let grouped_by_variable ~raise () =
  let () = first_test ~raise () in
  let () = second_test ~raise () in
  let () = ctor_add_and_merge ~raise () in
  let () = ctor_add_and_remove ~raise () in
  let () = row_add_and_merge ~raise () in
  let () = row_add_and_remove ~raise () in
  let () = poly_add_and_merge ~raise () in
  let () = poly_add_and_remove ~raise () in
  let () = access_label_add_and_merge ~raise () in
  let () = access_label_add_and_remove ~raise () in
  let () = mixed ~raise () in
  ()
