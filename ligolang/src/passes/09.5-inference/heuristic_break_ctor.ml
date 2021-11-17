(* selector / propagation rule for breaking down composite types
 * For now: break pair(a, b) = pair(c, d) into a = c, b = d *)

module TYPE_VARIABLE_ABSTRACTION = Type_variable_abstraction.TYPE_VARIABLE_ABSTRACTION

module INDEXES = functor (Type_variable : sig type t end) (Type_variable_abstraction : TYPE_VARIABLE_ABSTRACTION(Type_variable).S) -> struct
  module All_plugins = Database_plugins.All_plugins.M(Type_variable)(Type_variable_abstraction)
  open All_plugins
  module type S = sig
    val grouped_by_variable : Type_variable.t Grouped_by_variable.t
  end
end

open Trace
open Typer_common.Errors

module M = functor (Type_variable : sig type t end) (Type_variable_abstraction : TYPE_VARIABLE_ABSTRACTION(Type_variable).S) -> struct
  open Type_variable_abstraction.Types
  open Type_variable_abstraction.Misc
  open Type_variable_abstraction.Reasons
  type type_variable = Type_variable.t

  type selector_output = {
    a_k_var : constructor_or_row ;
    a_k'_var' : constructor_or_row ;
  }

  type flds = (module INDEXES(Type_variable)(Type_variable_abstraction).S)
  module All_plugins = Database_plugins.All_plugins.M(Type_variable)(Type_variable_abstraction)
  open All_plugins

  let heuristic_name = "break_ctor"

let selector : (type_variable -> type_variable) -> type_constraint_simpl -> flds -> selector_output list =
  (* find two rules with the shape x = k(var …) and x = k'(var' …) *)
  fun (repr : (type_variable -> type_variable)) type_constraint_simpl ((module Indexes) : flds) ->
    match type_constraint_simpl with
    | SC_Apply _ -> []
    | SC_Abs   _ -> []
    | SC_Constructor c -> (
      (* Format.eprintf "In break_ctor.selector_ for %a\n%!" Type_variable_abstraction.PP.type_constraint_simpl_short type_constraint_simpl;*)
      (* finding other constraints related to the same type variable and
      with the same sort of constraint (constructor vs. constructor)
      is symmetric *)
      let other_rows_lhs = Grouped_by_variable.get_rows_by_lhs (repr c.tv) Indexes.grouped_by_variable in
      let tmp = Grouped_by_variable.get_constructors_by_lhs (repr c.tv) Indexes.grouped_by_variable in
      let other_constructors_lhs = 
        List.filter ~f:(fun x -> not @@  (Type_variable_abstraction.Compare.c_constructor_simpl c x = 0)) @@ MultiSet.elements @@
        tmp in
      (* Format.eprintf "Other constructor : (%a)\n%!" Type_variable_abstraction.PP.(list_sep_d c_constructor_simpl_short) other_constructors_lhs; *)
      let () = ( if MultiSet.is_empty other_rows_lhs
                 then ()
                 else failwith (Format.asprintf "TODO: type error with %a ; %a" Type_variable_abstraction.PP.c_constructor_simpl c (MultiSet.pp Type_variable_abstraction.PP.c_row_simpl) other_rows_lhs))
      in    
      let cs_pairs = List.map ~f:(fun x -> { a_k_var = `Constructor c ; a_k'_var' = `Constructor x }) other_constructors_lhs in
      cs_pairs
    )
    | SC_Alias       _                -> []
    | SC_Typeclass   _                -> []
    | SC_Access_label _               -> []
    | SC_Poly        _                -> []
    | SC_Row         r                -> (
      (* Format.eprintf "In break_ctor.selector_ for %a\n%!" Type_variable_abstraction.PP.type_constraint_simpl_short type_constraint_simpl; *)
      let other_rows_lhs = 
        List.filter ~f:(fun x -> not @@  (Type_variable_abstraction.Compare.c_row_simpl r x = 0)) @@ MultiSet.elements @@
        Grouped_by_variable.get_rows_by_lhs (repr r.tv) Indexes.grouped_by_variable in
      let constructors_lhs = Grouped_by_variable.get_constructors_by_lhs (repr r.tv) Indexes.grouped_by_variable in
      let () = ( if MultiSet.is_empty constructors_lhs
                 then ()
                 else failwith (Format.asprintf "TODO: type error with %a ; %a" Type_variable_abstraction.PP.c_row_simpl r (MultiSet.pp Type_variable_abstraction.PP.c_constructor_simpl) constructors_lhs)) in
      let cs_pairs = List.map ~f:(fun x -> { a_k_var = `Row r ; a_k'_var' = `Row x }) other_rows_lhs in
      cs_pairs
    )

(* when a = k(…) and b = k'(…) are in the db, aliasing a and b should
   check if they're non-empty (and in that case produce a
   selector_output for all pairs / more efficiently any single pair
   since the break_ctor creates equivalence classes for the
   constructor arguments) *)

let alias_selector : type_variable -> type_variable -> flds -> selector_output list =
  fun a b (module Indexes) ->
  (* Format.eprintf "Break_ctor.alias_selector %a %a\n%!" Type_variable_abstraction.PP.type_variable a Type_variable_abstraction.PP.type_variable b ; *)
  let a_constructors = Grouped_by_variable.get_constructors_by_lhs a Indexes.grouped_by_variable in
  let b_constructors = Grouped_by_variable.get_constructors_by_lhs b Indexes.grouped_by_variable in
  let a_rows = Grouped_by_variable.get_rows_by_lhs a Indexes.grouped_by_variable in
  let b_rows = Grouped_by_variable.get_rows_by_lhs b Indexes.grouped_by_variable in
  let a_ctor = MultiSet.map_elements (fun a -> `Constructor a) a_constructors in
  let b_ctor = MultiSet.map_elements (fun a -> `Constructor a) b_constructors in
  let a_row = List.map ~f:(fun a -> `Row a) (MultiSet.elements a_rows) in
  let b_row = List.map ~f:(fun a -> `Row a) (MultiSet.elements b_rows) in
  match a_ctor @ a_row with
  | [] -> []
  | old_ctors_hd :: _ ->
    (match b_ctor @ b_row with
       [] -> []
     | new_ctors_hd :: _ ->
       [{ a_k_var = old_ctors_hd ; a_k'_var' = new_ctors_hd }])

let get_referenced_constraints ({ a_k_var; a_k'_var' } : selector_output) : type_constraint_simpl list =
  [
    (match a_k_var with `Constructor c -> SC_Constructor c | `Row r -> SC_Row r);
    (match a_k'_var' with `Constructor c -> SC_Constructor c | `Row r -> SC_Row r);
  ]

let printer ppf {a_k_var;a_k'_var'} =
  let open Format in
  let open Type_variable_abstraction.PP in
  fprintf ppf "%a = %a"
    constructor_or_row_short a_k_var
    constructor_or_row_short a_k'_var'
let printer_json ({a_k_var;a_k'_var'}) =
  let open Type_variable_abstraction.Yojson in
  `Assoc [
    ("a_k_var", constructor_or_row a_k_var);
    ("a_k'_var'", constructor_or_row a_k'_var')]
let comparator { a_k_var=a1; a_k'_var'=a2 } { a_k_var=b1; a_k'_var'=b2 } =
  let open Type_variable_abstraction.Compare in
  constructor_or_row a1 b1 <? fun () -> constructor_or_row a2 b2

let propagator : (selector_output, _) Type_variable_abstraction.Solver_types.propagator =
  fun ~raise selected repr ->
  (* Format.eprintf "In break_ctor.propagator for %a\n%!" printer selected; *)
  let a = selected.a_k_var in
  let b = selected.a_k'_var' in
  let get_tv : constructor_or_row -> type_variable = fun cr ->
    match cr with
    | `Row r -> repr r.tv
    | `Constructor c -> repr c.tv
  in
  (* The selector is expected to provice two constraints with the shape x = k(var …) and x = k'(var' …) *)
  let a_tv = repr @@ get_tv a in
  let b_tv = repr @@ get_tv b in
  assert (Type_variable_abstraction.Compare.type_variable a_tv b_tv = 0);
  (* produce constraints: *)
  (* a.tv = b.tv *) (* nope, already the same *)
  (* let eq1 = c_equation (wrap (Propagator_break_ctor "a") @@ P_variable a_tv) (wrap (Propagator_break_ctor "b") @@ P_variable b_tv) "propagator: break_ctor" in *)
  (* let () = if Type_variable_abstraction.Debug.debug_new_typer then
      let p = Type_variable_abstraction.PP.c_constructor_simpl in
      Printf.fprintf stderr "%s" @@ Format.asprintf "\npropagator_break_ctor\na = %a\nb = %a\n%!" p a p b in *)
  (* a.c_tag = b.c_tag *)
  ( match a , b with
    | `Row a , `Row b ->
      if (Type_variable_abstraction.Compare.row_tag a.r_tag b.r_tag) <> 0 then
        (* TODO : use error monad *)
        failwith (Format.asprintf "type error: incompatible types, not same ctor %a vs. %a (compare returns %d)"
                    Type_variable_abstraction.PP.c_row_simpl a
                    Type_variable_abstraction.PP.c_row_simpl b
                    (Type_variable_abstraction.Compare.row_tag a.r_tag b.r_tag))
    | `Constructor a , `Constructor b ->
      if (Type_variable_abstraction.Compare.constant_tag a.c_tag b.c_tag) <> 0 then
        (* TODO : use error monad *)
        failwith (Format.asprintf "type error: incompatible types, not same ctor %a vs. %a (compare returns %d)"
                    Type_variable_abstraction.PP.c_constructor_simpl a
                    Type_variable_abstraction.PP.c_constructor_simpl b
                    (Type_variable_abstraction.Compare.constant_tag a.c_tag b.c_tag))
    | _ -> failwith "type error : break_ctor propagator"
  );
  (* Produce constraint a.tv_list = b.tv_list *)
  let eqs3 =
    match a , b with
    | `Row a , `Row b ->
      let aux = fun ((la,{associated_variable=aa;_}),(lb,{associated_variable=bb;})) ->
        let () = Trace.Assert.assert_true ~raise (corner_case "TODO: different labels la lb") (Type_variable_abstraction.Compare.label la lb = 0) in
        c_equation
          (wrap (Propagator_break_ctor "a") @@ P_variable (repr aa))
          (wrap (Propagator_break_ctor "b") @@ P_variable (repr bb))
          "propagator: break_ctor: row"
      in
      let bindings =  
        match List.map2 ~f:(fun x y -> (x,y)) (LMap.bindings a.tv_map) (LMap.bindings b.tv_map)
        with Ok a -> a | List.Or_unequal_lengths.Unequal_lengths -> raise.raise @@ (corner_case "TODO: different number of labels (List.length a.tv_map) (List.length b.tv_map)")
      in
      List.map ~f:aux bindings
    | `Constructor a , `Constructor b -> (
      let aux = fun aa bb -> c_equation (wrap (Propagator_break_ctor "a") @@ P_variable (repr aa)) (wrap (Propagator_break_ctor "b") @@ P_variable (repr bb)) "propagator: break_ctor: ctor" in
        match List.map2 ~f:aux a.tv_list b.tv_list
        with Ok a -> a | List.Or_unequal_lengths.Unequal_lengths -> raise.raise @@ different_constant_tag_number_of_arguments __LOC__ a.c_tag b.c_tag (List.length a.tv_list) (List.length b.tv_list)
    )
    | _ -> failwith "type error in eqs3"
  in
  let eqs = eqs3 in
  (* Format.eprintf "Break_ctor : returning with new constraint %a\n%!" (PP_helpers.list_sep_d Type_variable_abstraction.PP.type_constraint_short) @@ eqs ; *)
  [
    {
      remove_constraints = [];
      add_constraints = eqs;
      add_constraints_simpl = [];
      proof_trace = Axiom Type_variable_abstraction.Axioms.f_equal
    }
  ]
end

module MM = M(Solver_types.Type_variable)(Solver_types.Opaque_type_variable)



open Ast_core.Types
open Solver_types

module Compat = struct
  include MM
  open All_plugins
  let compat_flds flds : MM.flds = (module struct
    let grouped_by_variable : type_variable Grouped_by_variable.t = flds#grouped_by_variable
  end)
  let selector repr c flds = MM.selector repr c (compat_flds flds)
  let alias_selector a b flds = MM.alias_selector a b (compat_flds flds)
end
let heuristic = Heuristic_plugin Compat.{ heuristic_name; selector; alias_selector; get_referenced_constraints; propagator; printer; printer_json; comparator }
type nonrec selector_output = MM.selector_output = {
  a_k_var : constructor_or_row ;
  a_k'_var' : constructor_or_row ;
}
let selector = Compat.selector
let propagator = Compat.propagator
let comparator = Compat.comparator
