open Trace
open Typer_common.Errors
open Ast_core.Types
open Solver_types

module M = functor (Solver_instance : sig type indexers_plugins_states end) -> struct
  open Solver_instance

module SRope = Rope.SimpleRope

let rec until predicate f state = if predicate state then state else let state = f state in until predicate f state

(* TODO: replace this with a more efficient SRope.t (needs a "pop" function) *)
module Pending = struct
  type 'a t = { l : 'a list }
  let empty : 'a t = { l = [] }
  let add : 'a -> 'a t -> 'a t = fun x { l } -> { l = x :: l }
  let add_list : 'a list -> 'a t -> 'a t = fun l1 { l } -> { l = l1 @ l }
  let union : 'a t -> 'a t -> 'a t = fun { l = l1 } { l = l2 } -> { l = l1 @ l2 }
  let of_list : 'a list -> 'a t = fun l -> { l }
  let singleton : 'a -> 'a t = fun x -> { l = [x] }
  let to_list : 'a t -> 'a list = fun { l } -> l
  let pop : 'a t -> ('a * 'a t) option  = function
      { l = [] } -> None
    | { l = hd :: tl } -> Some (hd, { l = tl })
  let is_empty : 'a t -> bool = function { l = [] } -> true | _ -> false
end

module type PENDING_PROPAGATOR = sig
  type a
  val heuristic_plugin : (a, indexers_plugins_states) heuristic_plugin
  val selector_output : a
end

type worklist_ = {
  (* I don't know how to "open" the module to get only the fields for { w with … = … } or w.…, so I'm declaring this outside of the module. *)
  pending_type_constraint                        : type_constraint       Pending.t;
  pending_filtered_not_already_added_constraints : type_constraint       Pending.t;
  pending_type_constraint_simpl                  : type_constraint_simpl Pending.t;
  pending_c_alias                                : c_alias               Pending.t;
  pending_non_alias                              : type_constraint_simpl Pending.t;
  pending_hc                                     : (type_constraint_simpl * indexers_plugins_states ex_heuristic_state * ((indexers_plugins_states ex_heuristic_state as 'a) list -> 'a -> 'a list)) Pending.t;
  pending_propagators                            : (module PENDING_PROPAGATOR)       Pending.t;
  pending_updates                                : update                Pending.t;
  pending_removes                                : type_constraint_simpl Pending.t
}

type ('part, 'whole) mini_lens = { get : 'whole -> 'part Pending.t; set : 'whole -> 'part Pending.t -> 'whole; }   

module Worklist = struct
  type t = worklist_
  type monad =
      Some_processing_done of t
    | Unchanged of t

  let decrement_has_timeout_expired time_to_live = 
    let () = time_to_live := !time_to_live - 1 in
    if (!time_to_live) = 0
    then (Format.eprintf "timeout 78909765.\n"; true)
    else false

  let show_sizes 
      { pending_type_constraint;
        pending_filtered_not_already_added_constraints;
        pending_type_constraint_simpl;
        pending_c_alias;
        pending_non_alias;
        pending_hc;
        pending_propagators;
        pending_updates;
        pending_removes } =
    if Ast_core.Debug.debug_new_typer then Printf.fprintf stderr "size(worklist)=(%d | %d | %d | %d | %d | %d | %d | %d | %d)\n"
      (List.length @@ Pending.to_list pending_type_constraint                       )
      (List.length @@ Pending.to_list pending_filtered_not_already_added_constraints)
      (List.length @@ Pending.to_list pending_type_constraint_simpl                 )
      (List.length @@ Pending.to_list pending_c_alias                               )
      (List.length @@ Pending.to_list pending_non_alias                             )
      (List.length @@ Pending.to_list pending_hc                                    )
      (List.length @@ Pending.to_list pending_propagators                           )
      (List.length @@ Pending.to_list pending_updates                               )
      (List.length @@ Pending.to_list pending_removes                               )
      
  let is_empty ~time_to_live
      (_state,
       ({ pending_type_constraint;
          pending_filtered_not_already_added_constraints;
          pending_type_constraint_simpl;
          pending_c_alias;
          pending_non_alias;
          pending_hc;
          pending_propagators;
          pending_updates;
          pending_removes })) =
    (* let () = show_sizes worklist in *)
    decrement_has_timeout_expired time_to_live
    || (Pending.is_empty pending_type_constraint                        &&
        Pending.is_empty pending_filtered_not_already_added_constraints &&
        Pending.is_empty pending_type_constraint_simpl                  &&
        Pending.is_empty pending_c_alias                                &&
        Pending.is_empty pending_non_alias                              &&
        Pending.is_empty pending_hc                                     &&
        Pending.is_empty pending_propagators                            &&
        Pending.is_empty pending_updates                                &&
        Pending.is_empty pending_removes                                 )

  let process ~raise lens f (state, worklist) =
    match (Pending.pop (lens.get worklist)) with
      None -> (state, Unchanged worklist)
    | Some (element, rest) ->
      (* set this field of the worklist to the rest of this Pending.t *)
      let worklist = lens.set worklist rest in
      (* Process the element *)
      let (state, new_worklist) = f ~raise (state, element) in
      (* While processing, f can queue new tasks in a fresh worklist, we're merging the worklists here *)
      let merged_worklists = {
        pending_type_constraint                        = Pending.union new_worklist.pending_type_constraint                        worklist.pending_type_constraint                        ;
        pending_filtered_not_already_added_constraints = Pending.union new_worklist.pending_filtered_not_already_added_constraints worklist.pending_filtered_not_already_added_constraints ;
        pending_type_constraint_simpl                  = Pending.union new_worklist.pending_type_constraint_simpl                  worklist.pending_type_constraint_simpl                  ;
        pending_c_alias                                = Pending.union new_worklist.pending_c_alias                                worklist.pending_c_alias                                ;
        pending_non_alias                              = Pending.union new_worklist.pending_non_alias                              worklist.pending_non_alias                              ;
        pending_hc                                     = Pending.union new_worklist.pending_hc                                     worklist.pending_hc                                     ;
        pending_propagators                            = Pending.union new_worklist.pending_propagators                            worklist.pending_propagators                            ;
        pending_updates                                = Pending.union new_worklist.pending_updates                                worklist.pending_updates                                ;
        pending_removes                                = Pending.union new_worklist.pending_removes                                worklist.pending_removes                                ;
      }
      (* return the state updated by f, and the updated worklist (without the processed element, with the new tasks) *)
      in (state, Some_processing_done merged_worklists)

  
  let rec process_all ~raise ~time_to_live lens f (state, worklist) =
    if decrement_has_timeout_expired time_to_live
    then (state, Unchanged worklist)
    else
      let (state, worklist) = process ~raise lens f (state, worklist) in
      match worklist with
        Some_processing_done worklist ->
        let (state, worklist) = process_all ~raise ~time_to_live lens f (state, worklist) in
        (match worklist with
           Some_processing_done worklist ->
           (state, Some_processing_done worklist)
         | Unchanged worklist ->
           (state, Some_processing_done worklist))
      | Unchanged worklist ->
        (state, Unchanged worklist)

  let empty = {
    (* TODO: these should be ropes *)
    pending_type_constraint                        = Pending.empty ;
    pending_filtered_not_already_added_constraints = Pending.empty ;
    pending_type_constraint_simpl                  = Pending.empty ;
    pending_c_alias                                = Pending.empty ;
    pending_non_alias                              = Pending.empty ;
    pending_hc                                     = Pending.empty ;
    pending_propagators                            = Pending.empty ;
    pending_updates                                = Pending.empty ;
    pending_removes                                = Pending.empty ;
  }
end

let pending_type_constraint                        = { get = (fun { pending_type_constraint                        = x ; _ } -> x); set = (fun w x -> { w with pending_type_constraint                        = x }) }
let pending_filtered_not_already_added_constraints = { get = (fun { pending_filtered_not_already_added_constraints = x ; _ } -> x); set = (fun w x -> { w with pending_filtered_not_already_added_constraints = x }) }
let pending_type_constraint_simpl                  = { get = (fun { pending_type_constraint_simpl                  = x ; _ } -> x); set = (fun w x -> { w with pending_type_constraint_simpl                  = x }) }
let pending_c_alias                                = { get = (fun { pending_c_alias                                = x ; _ } -> x); set = (fun w x -> { w with pending_c_alias                                = x }) }
let pending_non_alias                              = { get = (fun { pending_non_alias                              = x ; _ } -> x); set = (fun w x -> { w with pending_non_alias                              = x }) }
let pending_hc                                     = { get = (fun { pending_hc                                     = x ; _ } -> x); set = (fun w x -> { w with pending_hc                                     = x }) }
let pending_propagators                            = { get = (fun { pending_propagators                            = x ; _ } -> x); set = (fun w x -> { w with pending_propagators                            = x }) }
let pending_updates                                = { get = (fun { pending_updates                                = x ; _ } -> x); set = (fun w x -> { w with pending_updates                                = x }) }
let pending_removes                                = { get = (fun { pending_removes                                = x ; _ } -> x); set = (fun w x -> { w with pending_removes                                = x }) }


let rec until' ~raise :
  (* predicate      *) ('state * Worklist.t -> bool) ->
  (* f              *) (raise:typer_error raise -> 'state * Worklist.t -> 'state * Worklist.monad) ->
  (* state,worklist *) 'state * Worklist.t ->
  (* returns:       *) 'state * Worklist.t
  = fun predicate f ((state : 'state), (worklist : Worklist.t)) ->
    if predicate (state, worklist) then
      (state, worklist)
    else
      let (state, worklist_monad) = f ~raise (state, worklist) in
      match worklist_monad with
        Worklist.Unchanged w ->
        (if predicate (state, w) then
           (state, w)
         else raise.raise (solver_made_no_progress "inside 'until': no worklist worker was triggered"))
      | Worklist.Some_processing_done w ->
        until' ~raise predicate f (state, w)

let rec choose_processor' : 'typer_state 'typer_error . raise:'typer_error raise -> (raise:'typer_error raise -> 'typer_state * Worklist.t -> 'typer_state * Worklist.monad) list -> 'typer_state * Worklist.monad -> 'typer_state * Worklist.monad =
  fun ~raise processors (state, worklist) ->
  match processors with
    [] -> (state, worklist)
  | hd::tl ->
    match worklist with
      Worklist.Some_processing_done worklist ->
      (state, Worklist.Some_processing_done worklist)
    | Worklist.Unchanged worklist ->
      let (state, worklist) =
        hd ~raise (state, worklist)
      in choose_processor' ~raise tl (state, worklist)

let choose_processor : 'typer_state 'typer_error .raise:'typer_error raise -> (raise:'typer_error raise -> 'typer_state * Worklist.t -> 'typer_state * Worklist.monad) list -> 'typer_state * Worklist.t -> 'typer_state * Worklist.monad =
  fun ~raise processors (state, worklist) -> choose_processor' ~raise processors (state, Worklist.Unchanged worklist)

(* module Worklist_monad = struct

  module Let_syntax = struct
    let bind some_result ~f =
      let (state, (worklist_monad : Worklist.monad)) = some_result in
      match worklist_monad with
        Worklist.Some_processing_done w -> ok (state, Worklist.Some_processing_done w)
      | Worklist.Unchanged w -> f (state, w)
  end
end *)
end
