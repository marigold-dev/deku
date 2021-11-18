module Core = Typesystem.Core
open Ast_core.Types
open Ast_core.Reasons
open Trace
let test_err s = Main_errors.test_internal s
let tst_assert s p = Assert.assert_true (test_err s) p

let mk p_ctor_tag p_ctor_args = (wrap (Todo "unit test") @@ P_constant { p_ctor_tag ; p_ctor_args ; })
(* A bunch of arbitrary types (they only need to be distrinct type constructors without arguments, feel free to replace the contents if/when some of these types move to the stdlib and aren't built-in anymore). *)
let (int, unit, nat, string, bytes, mutez) = (mk C_int [], mk C_unit [], mk C_nat [], mk C_string [], mk C_bytes [], mk C_mutez [])
(* An arbitrary two-argument type constructor (this only needs to be a type constructor with two arguments, feel free to replace). *)
let map (k,v) = mk C_map [k; v]
(* A bunch of type variables: *)
let (m,n,n',o,p,q,r,x,y,z,v) = let v name : type_variable = Var.fresh ~name () in v "m", v "n", v "n'", v "o", v "p", v "q", v "r", v "x", v "y", v "z", v "v"
let var x = wrap (Todo "unit test") @@ P_variable x

(* Syntactic sugar for human-readable constraints in the tests below. *)
let constraint_ id tv = (id, tv)
let (==) (id, tv) c = c id tv
let (===) a b = (a, b)
type 'a record_xy = { x : 'a; y : 'a }
let mk_row p_row_tag p_row_args = wrap (Todo "unit test") @@ P_row { p_row_tag ; p_row_args ; }
(* let record { x ; y } = mk_row C_record @@ *)
let braces_to_map { x; y } = LMap.of_list [ (Label "x", x) ; (Label "y", y) ]
(* type forall = Forall
 * let forall = Forall
 * let poly a = a *)
let forall a constraints = (a, constraints)
let (@=>) (binder, constraints) body id tv = (fun x -> SC_Poly x), { reason_poly_simpl = "unit test"; id_poly_simpl = ConstraintIdentifier.T id; original_id = None; tv; forall = { binder; constraints; body } }
let (@->) arg ret = wrap (Todo "unit test") @@ P_constant { p_ctor_tag = C_arrow; p_ctor_args = [arg;ret] }
let ctor c_tag tv_list id tv = (fun x -> SC_Constructor x), { reason_constr_simpl = "unit test"; id_constructor_simpl = ConstraintIdentifier.T id; original_id = None; tv; c_tag; tv_list; }
let row r_tag tv_map id tv = (fun x -> SC_Row x), { reason_row_simpl = "unit test"; id_row_simpl = ConstraintIdentifier.T id; original_id = None; tv; r_tag; tv_map = braces_to_map tv_map; }
let p_row r_tag tv_map = mk_row r_tag (braces_to_map tv_map)
let (!.) (f, x) = f x (* Just circumvent some variant vs. variant case issue in OCaml… *)
let (!..) (_f, x) = x (* Just circumvent some variant vs. variant case issue in OCaml… *)

let grouped_by_variable state =
  let open Database_plugins.GroupedByVariable in
  object
    method grouped_by_variable =
      List.fold_left ~f:(add_constraint (fun v -> v)) ~init:(create_state ~cmp:Var.compare) state
  end

let check_list_of_equalities ~raise (la : update list) (_lb : (type_variable * type_variable) list) =
  let aux' ({ reason = _; c } : type_constraint) =
    match c with
    | Ast_core.Types.C_equation { aval = { location = _; wrap_content = aval_ } ; bval = { location = _; wrap_content = bval_ } } ->
      (match aval_, bval_ with
         P_variable avar, P_variable bvar -> (if Var.equal avar bvar then [] else [avar; bvar])
       | _ ->
         raise.raise (test_err "bad result from heuristic_break_ctor, expected equation constraint with two variables, but at least one of the members of the equation is not a variable."))
    | Ast_core.Types.C_typeclass _ ->
      raise.raise (test_err "bad result from heuristic_break_ctor, expected equation constraints but got got a typeclass constraint.")
    | Ast_core.Types.C_access_label _ ->
      raise.raise (test_err "bad result from heuristic_break_ctor, expected equation constraints but got got a c_access_label constraint.")
    | Ast_core.Types.C_apply _ ->
      raise.raise (test_err "bad result from heuristic_break_ctor, expected equation constraints but got got a c_apply.") in
  let aux ({ remove_constraints; add_constraints; proof_trace } : update) =
    let () = tst_assert ~raise "bad result from heuristic_break_ctor, expected no constraints to remove but got some." (List.length remove_constraints = 0) in
    ignore proof_trace;
    List.map ~f:aux' add_constraints in
  let assignments = List.map ~f:aux la in
  let assignments' = List.concat assignments in
  let assignments'' = List.filter ~f:(function [] -> false | _ -> true) assignments' in
  let assignments''' = List.map ~f:(List.sort ~compare:Var.compare) assignments'' in
  let assignments'''' = List.sort ~compare:(List.compare Var.compare) assignments''' in
  let expected' = List.map ~f:(fun (a, b) -> [a;b]) _lb in
  let expected'' = List.map ~f:(List.sort ~compare:Var.compare) expected' in
  let expected''' = List.sort ~compare:(List.compare Var.compare) expected'' in
  let msg : string = "expected a list of constraints: [(m = y); (n = z)] (order is not important, order lhs/rhs in the equality is not important) but got"
                     ^ (Format.asprintf "%a" (PP_helpers.list_sep_d_par (PP_helpers.list_sep_d_par Var.pp)) assignments'''')
                     ^ " expected: "
                     ^ (Format.asprintf "%a" (PP_helpers.list_sep_d_par (PP_helpers.list_sep_d_par Var.pp)) expected''')
  in
  tst_assert ~raise msg @@ (List.compare (List.compare Var.compare) expected''' assignments'''' = 0)
