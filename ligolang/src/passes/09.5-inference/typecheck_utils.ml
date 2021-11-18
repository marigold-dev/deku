open Trace
open Typer_common.Errors
open Ast_core.Types

open Pretty_print_variables
open Solver_types

module All_vars(Plugins : Plugins) = struct
  module Plugin_states = Plugins.Indexers.Indexers_plugins_fields(PerPluginState)
  let all_vars (state : 'plugin_states typer_state) =
    let from_aliases = List.concat @@ UnionFind.Poly2.partitions state.aliases in
    let aux1 : type_variable * constructor_or_row  -> type_variable list = fun (tv, cor) ->
      match cor with
      | `Constructor k -> tv :: k.tv :: k.tv_list
      | `Row r -> tv :: r.tv :: List.map ~f:(fun {associated_variable} -> associated_variable) (LMap.to_list r.tv_map) in
    let from_assignments = List.concat @@ List.map
      ~f:aux1
      (Plugin_states.Assignments.bindings (Plugin_states.assignments state.plugin_states)#assignments)
    in
    let type_constraint_simpl_all_vars = function
    | SC_Apply       a  -> [a.f; a.arg]                       (* φ(β) *)
    | SC_Abs         a  -> [a.tv;]                            (* α = λβ.τ *)
    | SC_Constructor k  -> k.tv :: k.tv_list                  (* α = ctor(β, …) *)
    | SC_Alias       al -> [al.a ; al.b]                      (* α = β *)
    (* the binder β is not a unification variable of the program,
        and the δ is a type expression that needs to be specialized
      before it is used, so we only return the p.tv *)
    | SC_Poly        p  -> [p.tv]                             (* α = forall β, δ where δ can be a more complex type *)
      (* the allowed types are not actually part of the program,
          so only the args are returned *)
    | SC_Typeclass   tc -> tc.args                            (* TC(α, …) *)
    | SC_Access_label l  -> [l.tv; l.record_type]                   (* TC(α, …) *)
    | SC_Row         r  -> r.tv :: List.map ~f:(fun {associated_variable} -> associated_variable) (LMap.to_list r.tv_map)      (* α = row(l -> β, …) *)
    in
    let from_constraints = List.concat @@ List.map ~f:type_constraint_simpl_all_vars @@ PolySet.elements state.all_constraints in
    let uniq lst = PolySet.elements (PolySet.add_list lst (PolySet.create ~cmp:Var.compare)).set in
    uniq (from_aliases @ from_assignments @ from_constraints)
end

let other_check all_constraints assignments =
  ignore all_constraints ; ignore assignments ;
  "check that all_constraints are referencing only variables which are either bound by a forall or assigned in the assignments,\n\
  and check that the variables referenced by the assignments are also assigned or bound by a forall within their scope (how do we even know that scope thing????\n\
  leave as TODO for now. Also, todo: call this in the solver somewhere."

  (*let check_variable : type_variable -> (type_variable -> constructor_or_row option) -> unit result =
    fun v find_assignment *)

  let rec toposort : raise:('a raise) -> (type_variable PolySet.t) -> 
                     (*initial set of vertices:*) type_variable -> (type_variable -> type_variable) ->
                     (*edges:*)                   (type_variable(*vertex*) -> constructor_or_row(*list of neigbours*) option) ->
                 (type_variable PolySet.t * type_variable Compare_renaming.tree)
    = fun ~raise already_seen unification_var repr find_assignment ->
      let repr_unification_var = repr unification_var in
      (* Format.eprintf "In toposort for : %a , repr : %a\n%!" Ast_typed.PP.type_variable unification_var Ast_typed.PP.type_variable repr_unification_var; *)
      if Set.mem repr_unification_var already_seen then 
        (*return without further changes*) (already_seen, Compare_renaming.Node [])
      else (*add the dependencies first, then our newfound variable if it is still new*) (
        match find_assignment repr_unification_var with
        | Some c_or_r ->
          let args : type_variable list = match c_or_r with `Constructor k -> k.tv_list | `Row r -> List.map ~f:(fun {associated_variable} -> associated_variable) @@ LMap.to_list r.tv_map in
          let (already_seen, tree) = List.fold_map ~f:(fun already_seen tvar -> toposort ~raise already_seen tvar repr find_assignment) ~init:already_seen args in
          if Set.mem repr_unification_var already_seen then
            (already_seen, Compare_renaming.Node tree)
          else 
            (PolySet.add repr_unification_var already_seen,
                Compare_renaming.Node [ Node tree ; Leaf repr_unification_var ])
        | None ->
          let msg () = (Format.asprintf "TODO ERROR in typecheck_utils: unassigned variable %a" Var.pp repr_unification_var) in
          let () = queue_print (fun () -> Format.eprintf "%s" (msg())) in
          raise.raise (corner_case (msg ()))
      )

  let toposort ~raise :
    type_variable list -> (type_variable -> type_variable) -> (type_variable -> constructor_or_row option) -> (type_variable list) =
    fun unification_vars repr find_assignment ->
      let _, trees =
        List.fold_map
          ~f:(fun already_seen one_var -> toposort ~raise already_seen one_var repr find_assignment)
          ~init:(PolySet.create ~cmp:Var.compare)
          unification_vars
      in
      Compare_renaming.flatten_tree @@ Compare_renaming.Node trees

  type canon_constructor_or_row = [
    | `Constructor of < c_tag : Ast_core.constant_tag ; tv_list : type_variable list >
    | `Row         of < r_tag : Ast_core.row_tag      ; tv_map  : type_variable LMap.t >
  ]
  
  let hashcons ~raise =
    fun repr find_assignment (canon, first) unification_var ->
      (* unification_vars = [ α β χ δ . γ ] in a topologically sorted order
        solver_repr = { α↦α β↦β χ↦χ δ↦δ γ↦γ }
        find_assignment { α↦int() β↦bool() χ↦int() δ↦map(α,β) γ↦map(χ,β) }

        intermediate "canon" at the end = { int()↦α bool()↦β map(α,β)↦δ }
        intermediate "first" at the end { α↦α α↦χ β↦β δ↦δ γ↦γ }

        loop for the example point γ just after the "." in [ α β χ δ . γ ]:
          intermediate "canon" = { int()↦α bool()↦β map(α,β)↦δ }
          intermediate "first" { α↦α α↦χ β↦β δ↦δ }
          1) find_assignment(solver_repr(γ)) gives map(χ,β)                     else error "unassigned variable"
        *)
      let unification_var = (repr unification_var) in
      let assigment =
        match find_assignment unification_var with
        | Some cor -> cor
        | None -> raise.raise (corner_case "TODO ERROR 78906: unassigned variable")
      in
        (*
          2) look up UnionFind for each arg in χ,β
          2.1) PolyMap.find_opt χ first, gives α                                else internal error "variables are not in topological order"
          2.2) PolyMap.find_opt β first, gives β                                else internal error "variables are not in topological order"
        *)
      let lookup : type_variable -> type_variable = fun tv ->
        match PolyMap.find_opt (repr tv) first with
        | Some first_var_pointing_to_this_type -> first_var_pointing_to_this_type
        | None -> failwith "internal error: not in topo order" in

      (*  3) reconstruct canonical form of map(χ,β), this gives us map(α,β)     (* just copy the k.ctor_tag and replace the k.ctor_args, can't fail by itself *) *)
      let map_args f cor : canon_constructor_or_row = match cor with
        | `Constructor k -> `Constructor (object method c_tag = k.c_tag method tv_list = List.map ~f:f k.tv_list end)
        | `Row r -> `Row (object method r_tag = r.r_tag method tv_map = LMap.map (fun {associated_variable=v} -> f v) r.tv_map end)
      in
      let canon_repr : canon_constructor_or_row = map_args lookup assigment in
      (* 4) lookup map(α,β) in intemediate "canon", gives δ                    else add map(α,β)↦γ to the map "canon" and use γ *)
      let (canon, canon_var) =
        match PolyMap.find_opt canon_repr canon with
        | Some found_existing_identical_type -> canon, found_existing_identical_type
        | None -> PolyMap.add canon_repr unification_var canon, unification_var
      in
      (* 5) Add γ ↦ δ to map "first" *)
      let first = PolyMap.add unification_var canon_var first in
      (* intermediate result = the "canon" reverse mapping and the "first" mapping *)
      (canon, first)

  let hashcons ~raise : (type_variable list) ->
                 (type_variable -> type_variable) ->
                 (type_variable -> constructor_or_row option) ->
                 (* returns a union-find where variables which are effectively equal have the same repr *)
                 (type_variable, type_variable) PolyMap.t =
    fun unification_vars repr find_assignment ->
     (* do the loop here *)
     let (<?) ca cb = match ca with 0 -> cb () | c -> c in
     let compare_canon_constructor_or_row (a : canon_constructor_or_row) (b : canon_constructor_or_row) =
      match (a,b) with
      | (`Constructor k1, `Constructor k2) ->
        Ast_core.Compare.constant_tag k1#c_tag k2#c_tag
        <? fun () ->
        (* We know that variables are unique for their content here,
           so a simple Var.compare ensures equality of the whole
          type expression pointed to by that var *)
        List.compare Var.compare k1#tv_list k2#tv_list
      | (`Row         r1, `Row         r2) ->
        Ast_core.Compare.row_tag r1#r_tag r2#r_tag
        <? fun () ->
        (* We know that variables are unique for their content here,
           so a simple Var.compare ensures equality of the whole
          type expression pointed to by that var *)
        LMap.compare Ast_core.Compare.type_variable r1#tv_map r2#tv_map
      | (`Constructor _, `Row _) -> -1
      | (`Row _, `Constructor _) -> 1
    in
      let initial_canon = PolyMap.create ~cmp:compare_canon_constructor_or_row in
      let initial_first = PolyMap.create ~cmp:Var.compare in
      (* result = the "first" mapping *)
      let toposorted_unification_vars =  toposort ~raise unification_vars repr find_assignment in
      let (_canon,first) = List.fold ~f:(hashcons ~raise repr find_assignment) ~init:(initial_canon, initial_first) toposorted_unification_vars in
      first
