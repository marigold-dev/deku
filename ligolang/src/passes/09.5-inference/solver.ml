open Trace
open Typer_common.Errors
module Core = Typesystem.Core
module Map = RedBlackTrees.PolyMap
module Set = RedBlackTrees.PolySet
module UF = UnionFind.Poly2
open Ast_core.Types
open Solver_types
open Solver_helpers
open Proof_trace_checker

(* open Pretty_print_variables *)
module Formatt = Format
module SRope = Rope.SimpleRope

let logfile = stderr (* open_out "/tmp/typer_log" *)

(*  ………………………………………………………………………………………………… Plugin-based solver below ………………………………………………………………………………………………… *)

(* Later on, we'll ensure that all the heuristics register the
   existential/unification variables that they create, as well as the
   new constraints that they create. We will then check that they only
   use a small set of core axioms to derive new constraints, and
   produce traces justifying that instanciations satisfy all related
   constraints, and that all existential variables are instantiated
   (possibly by first generalizing the type and then using the
   polymorphic type argument to instantiate the existential). *)

module Make_solver(Plugins : Plugins) : sig
  type indexers_plugins_states = Plugins.Indexers.Indexers_plugins_fields(PerPluginState).flds
  type nonrec typer_state = indexers_plugins_states typer_state
  val pp_typer_state : raise:typer_error raise -> Format.formatter -> typer_state -> unit
  val get_alias : raise:typer_error raise -> Ast_core.type_variable -> type_variable poly_unionfind -> type_variable
  val main : raise:typer_error raise -> typer_state -> type_constraint list -> typer_state
  val initial_state : typer_state
  val placeholder_for_state_of_new_typer : unit -> typer_state
  val discard_state : typer_state -> unit
end = struct
  module Indexers_plugins_states = Plugins.Indexers.Indexers_plugins_fields(PerPluginState)
  type indexers_plugins_states = Plugins.Indexers.Indexers_plugins_fields(PerPluginState).flds
  type nonrec typer_state = indexers_plugins_states Solver_types.typer_state
  type indexers_plugins_fields_unit = Plugins.Indexers.Indexers_plugins_fields(PerPluginUnit).flds

  module Worklist_and_pending' = Worklist_and_pending.M(struct type nonrec indexers_plugins_states = indexers_plugins_states end)
  open Worklist_and_pending'
  
  module Solver_stages' = Solver_stages.M(Plugins)(struct
    module Indexers_plugins_states = Indexers_plugins_states
    type nonrec indexers_plugins_states = indexers_plugins_states
    type nonrec typer_state = typer_state
    (*type nonrec indexers_plugins_fields_unit = indexers_plugins_fields_unit*)
    module Worklist_and_pending = Worklist_and_pending'
  end)
  open Solver_stages'

  let indexers_plugins_fields_unit : indexers_plugins_fields_unit = Plugins.Indexers.indexers_plugins_fields_unit

  let pp_typer_state ~raise = fun ppf ({ all_constraints; added_constraints; deleted_constraints ; plugin_states; aliases ; already_selected_and_propagators } : typer_state) ->
    let open Solver_types in
    let open PP_helpers in
    let module MapPP = Plugins.Indexers.Map_indexer_plugins(PPPlugin) in
    let pp_indexers ppf states =
      Formatt.fprintf ppf "@[ <@ %a @ > @]" (fun ppf states -> let _ : indexers_plugins_fields_unit = MapPP.f ~raise ppf states in ()) states
    in
    Formatt.fprintf ppf "{@[<hv 2> @ all_constaints = %a;@ added_constraints = %a;@ deleted_constraints = %a;@ plugin_states = %a ;@ aliases = %a ;@ already_selected_and_propagators = %a @]@ }"
      (RedBlackTrees.PolySet.pp Ast_core.PP.type_constraint_simpl_short) all_constraints
      (RedBlackTrees.PolySet.pp Ast_core.PP.type_constraint_short) added_constraints
      (RedBlackTrees.PolySet.pp Ast_core.PP.type_constraint_simpl_short) deleted_constraints
      pp_indexers plugin_states
      (UnionFind.Poly2.pp Ast_core.PP.type_variable) aliases
      (list_sep pp_ex_propagator_state (fun ppf () -> Formatt.fprintf ppf " ;@ ")) already_selected_and_propagators

  let make_pending_propagators =
    (fun (type a) (heuristic_plugin : (a, indexers_plugins_states) heuristic_plugin) (selector_outputs : a list) : (module PENDING_PROPAGATOR) list ->
      List.map
      ~f:(fun (selector_output : a) ->
        (module struct
          type nonrec a = a
          let heuristic_plugin = heuristic_plugin
          let selector_output = selector_output
          end : PENDING_PROPAGATOR))
      selector_outputs)

  let aux_remove ~raise (state, to_remove) =
    (* let () = Formatt.eprintf "Remove constraint :\n  %a\n\n%!" Ast_core.PP.type_constraint_simpl_short to_remove in *)
    (* let () = Formatt.eprintf "and state:%a\n" pp_typer_state state in *)
    let module MapRemoveConstraint = Plugins.Indexers.Map_indexer_plugins(RemoveConstraint) in
    let plugin_states = MapRemoveConstraint.f ~raise (mk_repr state, to_remove) state.plugin_states in
    ({state with plugin_states ; deleted_constraints = PolySet.add to_remove state.deleted_constraints}, Worklist.empty)

  and aux_update ~raise:_ (state, { remove_constraints; add_constraints; add_constraints_simpl; proof_trace=_ }) =
    let open Ast_core.PP in
    let () = check_proof_trace proof_trace in
    let add_constraints_simpl = List.map ~f:(function
        SC_Apply c -> SC_Apply { c with id_apply_simpl = ConstraintIdentifier.fresh () }
      | SC_Abs c -> SC_Abs { c with id_abs_simpl = ConstraintIdentifier.fresh () }
      | SC_Constructor c -> SC_Constructor { c with id_constructor_simpl = ConstraintIdentifier.fresh () }
      | SC_Alias c -> SC_Alias c
      | SC_Access_label c -> SC_Access_label { c with id_access_label_simpl = ConstraintIdentifier.fresh () }
      | SC_Poly c -> SC_Poly { c with id_poly_simpl = ConstraintIdentifier.fresh () }
      | SC_Row c -> SC_Row { c with id_row_simpl = ConstraintIdentifier.fresh () }
      | SC_Typeclass c -> SC_Typeclass { c with id_typeclass_simpl = ConstraintIdentifier.fresh () }
      ) add_constraints_simpl in
    (* Format.eprintf "In aux_update, remove :%a, add:%a, add_simpl:%a\n%!" 
      (list_sep_d type_constraint_simpl_short) remove_constraints
      (list_sep_d type_constraint_short) add_constraints
      (list_sep_d type_constraint_simpl_short) add_constraints_simpl
      ; *)
    (* Format.eprintf "Returning from aux_update\n%!" ; *)
    (state, { Worklist.empty with pending_type_constraint = Pending.of_list add_constraints;
                                     pending_type_constraint_simpl = Pending.of_list add_constraints_simpl;
                                     pending_removes = Pending.of_list remove_constraints })

  and aux_propagator ~raise (state, (module M : PENDING_PROPAGATOR)) =
    let heuristic_plugin, selector_output = M.heuristic_plugin, M.selector_output in
    (* Format.eprintf "in aux_propagator %s for %a\n%!" heuristic_plugin.heuristic_name heuristic_plugin.printer selector_output; *)
    (* TODO: before applying a propagator, check if it does
       not depend on constraints which were removed by the
       previous propagator *)
    let referenced_constraints = heuristic_plugin.get_referenced_constraints selector_output in
    let uses_deleted_constraints = List.exists ~f:(fun c -> (PolySet.mem c state.deleted_constraints)) referenced_constraints in
    if uses_deleted_constraints then ( 
      (*Format.eprintf "contraint deleted; not runing propagator \n"; *)
      (state, Worklist.empty))
    else(
      (* Format.eprintf "reuning propagator : %s ..." heuristic_plugin.heuristic_name; *)
      let updates = heuristic_plugin.propagator ~raise selector_output (mk_repr state) in
      (state, { Worklist.empty with pending_updates = Pending.of_list @@ updates })
    )

  and add_alias ~raise : (typer_state * c_alias) -> typer_state * Worklist.t =

    let aux_selector_alias demoted_repr new_repr state (Heuristic_state heuristic) =
      let selector_outputs = heuristic.plugin.alias_selector demoted_repr new_repr state.plugin_states in
      let aux = fun (l,already_selected) el ->
        if PolySet.mem el already_selected then (l,already_selected)
        else (el::l, PolySet.add el already_selected)
      in
      let selector_outputs,already_selected = List.fold_left ~f:aux ~init:([], heuristic.already_selected) selector_outputs in
      let heuristic = { heuristic with already_selected } in
      Heuristic_selector (heuristic, selector_outputs)
    in
    fun (state , ({ reason_alias_simpl=_; a; b } as new_constraint)) ->
      (* Format.eprintf "Adding alias : %a\n%!" Ast_typed.PP.c_alias_short new_constraint; *)
      (* let () = Format.eprintf "Add_alias %a=%a\n%!" Ast_typed.PP.type_variable a Ast_typed.PP.type_variable b in *)


      let { all_constraints ; added_constraints ; deleted_constraints; plugin_states ; aliases ; already_selected_and_propagators } = state in
      
      (* get the changed reprs due to that alias constraint *)
      let UnionFind.Poly2.{ partition = aliases; changed_reprs } =
        UnionFind.Poly2.equiv a b aliases in
      (* let () = Format.eprintf "changed_reprs :(%a)\n%!" Ast_core.PP.(fun ppf ({demoted_repr=a;new_repr=b}: _ UnionFind.Poly2.changed_reprs) -> Format.fprintf ppf "%a -> %a" type_variable a type_variable b) changed_reprs in *)
      (* let () = Format.eprintf "New_aliases :%a\n%!" (UnionFind.Poly2.pp Ast_core.PP.type_variable) aliases in *)

      (*apply heuristics' selector*)
      let selected = List.map ~f:(aux_selector_alias UnionFind.Poly2.(changed_reprs.demoted_repr) UnionFind.Poly2.(changed_reprs.new_repr) state) already_selected_and_propagators in

      (* Add alias constraint to the set of all constraints *)
      let all_constraints = PolySet.add (SC_Alias new_constraint) all_constraints in

      let plugin_states =
        let module MapMergeAliases = Plugins.Indexers.Map_indexer_plugins(MergeAliases) in
        MapMergeAliases.f ~raise changed_reprs plugin_states in

      let state = { all_constraints ; added_constraints ; deleted_constraints; plugin_states ; aliases ; already_selected_and_propagators } in

      (* apply all the alias_selectors and propagators given the new alias *)
      (* This must be called before aliasing the variables in the database *)
      let pending_propagators = List.map
        ~f:(fun (Heuristic_selector (heuristic, selector_outputs)) -> make_pending_propagators heuristic.plugin selector_outputs)
        selected in
      (state, { Worklist.empty with pending_propagators = Pending.of_list @@ List.concat pending_propagators })
    

  and get_alias ~raise variable aliases =
    trace_option ~raise (corner_case (Format.asprintf "can't find alias root of variable %a" Var.pp variable)) @@
    (* TODO: after upgrading UnionFind, this will be an option, not an exception. *)
    try Some (UF.repr variable aliases) with Not_found -> None

  and aux_heuristic ~raise:_ (state, (constraint_, (Heuristic_state heuristic), set_heuristic_state)) =
    let repr = mk_repr state in
    let selector_outputs = heuristic.plugin.selector repr constraint_ state.plugin_states in
    (* Format.eprintf "Selected : %a\n%!" (PP_helpers.list_sep_d heuristic.plugin.printer) selector_outputs; *)
    
    let aux = fun (l,already_selected) el ->
      if PolySet.mem el already_selected then (l,already_selected)
      else (el::l, PolySet.add el already_selected)
    in
    let selector_outputs,already_selected = List.fold_left ~f:aux ~init:([], heuristic.already_selected) selector_outputs in
    (* Format.eprintf "Selected : %a, alredy_selected : %a\n%!" (PP_helpers.list_sep_d heuristic.plugin.printer) selector_outputs (PP_helpers.list_sep_d heuristic.plugin.printer) selector_outputs; *)
    let heuristic = { heuristic with already_selected } in
    let pending_propagators = make_pending_propagators heuristic.plugin selector_outputs in
    let state = { state with already_selected_and_propagators = set_heuristic_state state.already_selected_and_propagators (Heuristic_state heuristic) } in
    (* Format.eprintf "New state : %a" pp_typer_state state ; *)
    (state, { Worklist.empty with pending_propagators = Pending.of_list pending_propagators })

  (* apply all the selectors and propagators *)
  and add_constraint_and_apply_heuristics ~raise (state , constraint_) =
    (* let () = queue_print (fun () -> Format.eprintf "Add constraint and apply heuristics for constraint: %a\n%!" Ast_core.PP.type_constraint_simpl constraint_) in *)
    if PolySet.mem constraint_ state.all_constraints then (state, Worklist.empty)
    else
      let repr = mk_repr state in
      let module MapAddConstraint = Plugins.Indexers.Map_indexer_plugins(AddConstraint) in
      let state =
        { state with plugin_states = MapAddConstraint.f ~raise (repr, constraint_) state.plugin_states }
      in
      let hc = List.mapi ~f:(fun i asap -> constraint_, asap, List.set_nth i) state.already_selected_and_propagators in
      (state, { Worklist.empty with pending_hc = Pending.of_list hc })

  let pp_indented_constraint_list =
    let open PP_helpers in
    let open Ast_core.PP in
    (list_sep type_constraint_short (tag "\n  "))

  let pp_indented_constraint_simpl_list =
    let open PP_helpers in
    let open Ast_core.PP in
    (list_sep type_constraint_simpl_short (tag "\n  "))
  let _ = pp_indented_constraint_list, pp_indented_constraint_simpl_list (* unused warning *)

  (* Takes a list of constraints, applies all selector+propagator pairs
     to each in turn. *)
  let select_and_propagate_all ~raise : typer_state -> Worklist.t -> typer_state =
    fun state initial_constraints ->
    (* To change the order in which the constraints are processed, modify this loop. *)
    let () = if Ast_core.Debug.debug_new_typer then Formatt.eprintf "In select and propagate all\n" in
    let time_to_live = ref 50000 in
    until' ~raise
      (* repeat until the worklist is empty *)
      (Worklist.is_empty ~time_to_live)
      (fun ~raise (state, worklist) ->
         let () = if List.is_empty @@ Pending.to_list worklist.pending_type_constraint then () else
            if Ast_core.Debug.debug_new_typer then Formatt.eprintf "\nStart iteration with new constraints :\n  %a\n" pp_indented_constraint_list (Pending.to_list worklist.pending_type_constraint) in
         (* let () = Formatt.eprintf "and state: %a\n" pp_typer_state state in *)

         (* let () = queue_print (fun () -> Formatt.eprintf "Start iteration with constraints :\n  %a\n\n" pp_indented_constraint_list (Pending.to_list worklist.pending_type_constraint)) in *)

         choose_processor ~raise [
           (Worklist.process_all ~time_to_live pending_hc                                     aux_heuristic                      );
           (Worklist.process                   pending_non_alias                              add_constraint_and_apply_heuristics);
           (Worklist.process_all ~time_to_live pending_type_constraint                        filter_already_added               );
           (Worklist.process_all ~time_to_live pending_filtered_not_already_added_constraints simplify_constraint                );
           (Worklist.process_all ~time_to_live pending_type_constraint_simpl                  split_aliases                      );
           (Worklist.process_all ~time_to_live pending_removes                                aux_remove                         );
           (Worklist.process_all ~time_to_live pending_updates                                aux_update                         );
           (Worklist.process                   pending_c_alias                                add_alias                          );
           (Worklist.process                   pending_propagators                            aux_propagator                     );
           ]
           (state, worklist)
      )

      (state, initial_constraints)
    |> fst
  (* already_selected_and_propagators ; all_constraints ; plugin_states ; aliases *)

  module All_vars = Typecheck_utils.All_vars(Plugins)
  let main ~raise : typer_state -> type_constraint list -> typer_state =
    fun state initial_constraints ->
    let () = if Ast_core.Debug.debug_new_typer then Formatt.eprintf "In solver main\n%!" in
    let (state : typer_state) = select_and_propagate_all ~raise state {Worklist.empty with pending_type_constraint = Pending.of_list initial_constraints} in
    let () = if Ast_core.Debug.debug_new_typer then Formatt.eprintf "Starting typechecking with assignment :\n  %a\n%!"
      (pp_typer_state ~raise) state in
    let failure = Typecheck.check (PolySet.elements state.all_constraints)
      (All_vars.all_vars state)
      (fun v -> UnionFind.Poly2.repr v state.aliases)
      (fun v -> Indexers_plugins_states.Assignments.find_opt v (Indexers_plugins_states.assignments state.plugin_states)#assignments) in
    let () = if not @@ Trace.to_bool failure then Pretty_print_variables.flush_pending_print state in
    let () = failure ~raise in
    state
  
  (* This function is called when a program is fully compiled, and the
     typechecker's state is discarded. TODO: either get rid of the state
     earlier, or perform a sanity check here (e.g. that types have been
     inferred for all bindings and expressions, etc.

     Also, we should check at these places that we indeed do not need the
     state any further. Suzanne *)
  let discard_state (_ : typer_state) = ()

  let initial_state : typer_state =
    let module MapCreateState = Plugins.Indexers.Map_indexer_plugins(CreateState) in
    let plugin_states = MapCreateState.f ~raise:{raise=(fun _ -> failwith "corner_case")} () indexers_plugins_fields_unit in
    {
      all_constraints                  = PolySet.create ~cmp:Ast_core.Compare.type_constraint_simpl ;
      added_constraints                = PolySet.create ~cmp:Ast_core.Compare.type_constraint ;
      deleted_constraints              = PolySet.create ~cmp:Ast_core.Compare.type_constraint_simpl ;
      aliases                          = UnionFind.Poly2.empty Var.pp Var.compare ;
      plugin_states                    = plugin_states ;
      already_selected_and_propagators = List.map ~f:init_propagator_heuristic Plugins.heuristics ;
    }



  let placeholder_for_state_of_new_typer () = initial_state
end

(* TODO: make the typer a fonctor and move this instantiation as further outwards as possible. *)
(* Instantiate the solver with a selection of plugins *)
include Make_solver(Plugins)
type nonrec _ typer_state = typer_state

(*  ………………………………………………………………………………………………… Plugin-based solver above ………………………………………………………………………………………………… *)

let json_typer_state = fun ({ all_constraints=_ ; plugin_states=_ ; aliases=_ ; already_selected_and_propagators } : _ typer_state) : Yojson.Safe.t ->
  let open Solver_types in
  `Assoc[ ("all_constraints", `String "TODO");
          ("plugin_states", (* (Ast_core.Yojson.structured_dbs structured_dbs) *) `String "TODO");
          ("aliases", `String "TODO");
          ("already_selected_and_propagators",
           let list f lst = `List (List.map ~f:f lst) in
           (list json_ex_propagator_state already_selected_and_propagators))]
