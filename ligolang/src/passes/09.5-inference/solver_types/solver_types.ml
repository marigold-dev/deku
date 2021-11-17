open Trace
module Set = RedBlackTrees.PolySet

module TYPE_VARIABLE_ABSTRACTION = Type_variable_abstraction.TYPE_VARIABLE_ABSTRACTION

type ('selector_output , 'errors) propagator = raise:'errors raise -> 'selector_output -> (Ast_core.type_variable -> Ast_core.type_variable) -> Ast_core.updates

(* TODO: move this with the AST, probably? *)
module Axioms = Axioms
module Typelang = Typelang

module Type_variable = struct type t = Ast_core.Types.type_variable end

module Opaque_type_variable = struct
  let cast_access_to_simplifier_do_not_do_this_do_not_use_this = fun x -> x
  module Types        = Ast_core.Types
  module Substitution = Typesystem.Misc.Substitution
  module Compare      = Ast_core.Compare
  module PP           = Ast_core.PP
  module Yojson       = Ast_core.Yojson
  module Misc         = Ast_core.Misc
  module Reasons      = Ast_core.Reasons
  module Core         = Typesystem.Core
  module Axioms       = Axioms
  module Typelang     = Typelang
  module Errors       = Typer_common.Errors
  module Solver_types = struct type nonrec ('o, 'e) propagator = ('o, 'e) propagator end
end
module Check : Type_variable_abstraction.TYPE_VARIABLE_ABSTRACTION(Type_variable).S = Opaque_type_variable


(* ************ indexer plug-in system ************ *)

(* This plug-in system ensures the following:

 * the state of the plug-ins can only store unification vars in a few
   specific types (ReprMap.t and ReprSet.t)

 * when two unification variables are aliased, the plug-in is forced
   to update its state accordingly (it can either update each map/set
   with the supplied function or discard the entire map/set, but it
   cannot forget to update one of the maps/sets in its state; it is
   technically possible to modify the maps/sets (e.g. take a random
   value from one map/set and add it to another) but this would hardly
   be done accidentally).

 * the ReprMap and ReprSet modules only allow monotonic updates
   (additions but no deletions), unless one has access to the
   comparison function (which we do not provide to other modules), and
   the 'type_variable type is always quantified/hidden in positions
   where it could be used to remove from a map/set or completely empty
   it. *)

open UnionFind
(* The types are given in an approximative haskell, because its
   syntax is more readable and more consitent than OCaml's constant
   switch between module types and types *)

(* merge_keys contains functions which merge the given keys of a map, or elements of a set *)
(* data merge_keys old new = updater {
     map :: forall v . old → old → map old v → map new v
     set :: old → old → set old → set new
   }
*)
type ('old, 'new_) merge_keys = {
  map : 'v . ('old, 'v) ReprMap.t -> ('new_, 'v) ReprMap.t;
  set :       'old      ReprSet.t ->  'new_      ReprSet.t;
  (* var :       'old                ->  'new_               ; *)
}

(* Each normalizer returns an updated database (after storing the
   incoming constraint) and a list of constraints, used when the
   normalizer rewrites the constraints e.g. into simpler ones. *)
(* type normalizer state a b = state → a → (state, set b) *)
type ('state, 'a , 'b) normalizer = 'state -> 'a -> ('state * 'b PolySet.t)

(* type normalizer_rm state a = state → a → MonadError typer_error state *)
type ('state, 'a) normalizer_rm = raise:Typer_common.Errors.typer_error raise -> 'state -> 'a -> 'state

module INDEXER_PLUGIN_TYPE =
  functor
    (Type_variable : sig type t end)
    (Type_variable_abstraction : TYPE_VARIABLE_ABSTRACTION(Type_variable).S) ->
  struct
    (* t is the type of the state of the plugin
       data Plugin (t :: 🞰→🞰) = Plugin {
         create_state :: forall typeVariable . (typeVariable → typeVariable → int) → t typeVariable
         add_constraint :: normalizer (t typeVariable) type_constraint_simpl type_constraint_simpl
         remove_constraint :: normalizer_rm (t typeVariable) type_constraint_simpl
         merge_aliases :: forall old new . merge_keys old new → old → old → t old → t new
       }
    *)
    module type S = sig
      open Type_variable_abstraction.Types
      type 'typeVariable t
      (* Create the indexer's initial state *)
      val create_state : cmp:('typeVariable -> 'typeVariable -> int) -> 'typeVariable t
      (* Update the state when a constraint is added *)
      val add_constraint : ?debug:(Format.formatter -> 'type_variable -> unit) -> (Type_variable.t -> 'type_variable) -> 'type_variable t -> type_constraint_simpl -> 'type_variable t
      (* Update the state when a constraint is removed *)
      (* TODO: check this API to see if we're giving too much flexibility to the plugin *)
      val remove_constraint :
        raise:Type_variable_abstraction.Errors.typer_error raise ->
        (Format.formatter -> 'type_variable -> unit) ->
        (Type_variable.t -> 'type_variable) ->
        'type_variable t ->
        type_constraint_simpl ->
        'type_variable t
      (* Update the state to merge entries of maps and sets of type
         variables.  *)
      val merge_aliases : ?debug:(Format.formatter -> 'new_ t -> unit) -> ('old, 'new_) merge_keys -> 'old t -> 'new_ t
      (* The pretty-printer is used for debugging *)
      val pp : (Format.formatter -> 'typeVariable -> unit) -> Format.formatter -> 'typeVariable t -> unit
      val name : string
    end
  end

module type INDEXER_PLUGIN = sig
  module M : functor
    (Type_variable : sig type t end)
    (Type_variable_abstraction : TYPE_VARIABLE_ABSTRACTION(Type_variable).S) ->
    INDEXER_PLUGIN_TYPE(Type_variable)(Type_variable_abstraction).S
end

open Ast_core.Types            (* TODO: move this further down the file, after the big functors *)

(* The kind PerPluginType describes type-level functions which take
   a type Plugin.t, and produce an arbitrary type which can depend
   on it.

   e.g. given a module
     Ppt : PerPluginType
   the type
     Ppt.M(SomePlugin).t
   could be one of:
     type_variable SomePlugin.t
     int
     …
*)
(* type PerPluginType = 🞰→(🞰→🞰)→🞰 *)
module type PerPluginTypeArg = sig
  type 'type_variable t
  val pp : (Format.formatter -> type_variable -> unit) -> Format.formatter -> type_variable t -> unit
end (* just the part of Plugin we care about *)
module type PerPluginType = functor (Plugin : PerPluginTypeArg) -> sig
  type t
  val pp : Format.formatter -> t -> unit
end

(* These are two useful PerPlugin type-level functions. The first
   gives a `unit' type for each plugin, the second *)
module PerPluginUnit = functor (Plugin : PerPluginTypeArg) -> struct type t = unit let pp ppf () = Format.fprintf ppf "()" end
module PerPluginState = functor (Plugin : PerPluginTypeArg) -> struct type t = type_variable Plugin.t let pp ppf t = Format.fprintf ppf "%a" (Plugin.pp Ast_core.PP.type_variable) t end

module type Monad = sig
  type 'a t
  val return : 'a -> 'a t
  val bind : 'a t -> f:('a -> 'b t) -> 'b t
  val (let*) : 'a t -> ('a -> 'b t) -> 'b t
end

module NoMonad = struct
  type 'a t = 'a
  let return x = x
  let bind x ~f = f x
  let (let*)  x f = bind x ~f
end

(* type MappedFunction (t :: 🞰) (Plugin :: 🞰→🞰) =
     Plugin t → MakeInType t → MakeOutType t *)
module type Mapped_function = sig
  type extra_args
  module MakeInType : PerPluginType
  module MakeOutType : PerPluginType
  module F(Indexer_plugin : INDEXER_PLUGIN_TYPE(Type_variable)(Opaque_type_variable).S) : sig
    val f : raise:Typer_common.Errors.typer_error raise -> string -> extra_args -> MakeInType(Indexer_plugin).t -> MakeOutType(Indexer_plugin).t
  end
end

(* flds is an object containing one method per plug-in *)
module type Indexer_plugin_fields = functor (Ppt : PerPluginType) -> sig
  type flds

  (* The assignments plug-in must always be present. We force its
     inclusion by asking for a function extracting that field. *)
  module Assignments : sig
    type 'type_variable t
    val find_opt : 'type_variable -> 'type_variable t -> constructor_or_row option
    val bindings : 'type_variable t -> ('type_variable * constructor_or_row) list
    val pp : (Format.formatter -> 'type_variable -> unit) -> Format.formatter -> 'type_variable t -> unit
  end
  val assignments : flds -> < assignments : Ppt(Assignments).t >
end
module type IndexerPlugins = sig
  module Indexers_plugins_fields : Indexer_plugin_fields

  (* A default value where the field for each plug-in has type unit *)
  val indexers_plugins_fields_unit : Indexers_plugins_fields(PerPluginUnit).flds

  (* A function which applies F to each field *)
  module Map_indexer_plugins : functor (F : Mapped_function) ->
  sig
    val f :
      raise: Typer_common.Errors.typer_error raise ->
      F.extra_args ->
      (Indexers_plugins_fields(F.MakeInType).flds) ->
      (Indexers_plugins_fields(F.MakeOutType).flds)
  end
end

(* ************ end indexer plug-in system ************  *)

type ('selector_output, -'flds) selector = (type_variable -> type_variable) -> type_constraint_simpl -> 'flds -> 'selector_output list

type ('selector_output, -'flds) heuristic_plugin = {
  (* Finds in the databases tuples of constraints which are
     interesting for this plugin and include the given
     type_constraint_simpl. *)
  heuristic_name : string ;
  selector     : ('selector_output, 'flds) selector ;
  (* Select in the databases tuples of constraints which are
     interesting and involve the given two type_viables, knowing that
     these are about to be merged. This function is called before the
     database's merge_aliases functions are called (i.e. the database
     does not reflect the effects of the merge yet). *)
  alias_selector : type_variable -> type_variable -> 'flds -> 'selector_output list ;
  get_referenced_constraints : 'selector_output -> type_constraint_simpl list ;
  propagator   : ('selector_output , Typer_common.Errors.typer_error) propagator ;
  (* called when two 'data are associated with the same type_constraint *)
  printer      : Format.formatter -> 'selector_output -> unit ;
  printer_json : 'selector_output -> Yojson.Safe.t ;
  comparator   : 'selector_output -> 'selector_output -> int ;
}

type ('selector_output, -'flds) heuristic_state = {
  plugin : ('selector_output, 'flds) heuristic_plugin ;
  already_selected : 'selector_output Set.t ;
}

type -'flds ex_heuristic_plugin =
    Heuristic_plugin : ('selector_output, 'flds) heuristic_plugin -> 'flds ex_heuristic_plugin

type -'flds ex_heuristic_state =
    Heuristic_state : ('selector_output, 'flds) heuristic_state -> 'flds ex_heuristic_state

type -'flds ex_heuristic_selector =
    Heuristic_selector: ('selector_output, 'flds) heuristic_state * 'selector_output list -> 'flds ex_heuristic_selector

type 'flds heuristic_plugins = 'flds ex_heuristic_plugin list

type 'flds heuristic_states = 'flds ex_heuristic_state list

module type Plugins = sig
  module Indexers : IndexerPlugins
  val heuristics : Indexers.Indexers_plugins_fields(PerPluginState).flds heuristic_plugins
end

type ('plugin_states) typer_state = {
  all_constraints                  : type_constraint_simpl PolySet.t ;
  added_constraints                : type_constraint PolySet.t ;
  deleted_constraints              : type_constraint_simpl PolySet.t ;
  aliases                          : type_variable UnionFind.Poly2.t ;
  plugin_states                    : 'plugin_states ;
  already_selected_and_propagators : 'plugin_states ex_heuristic_state list ;
}

open Format
open PP_helpers

let pp_already_selected = fun printer ppf set ->
  let lst = (RedBlackTrees.PolySet.elements set) in
    Format.fprintf ppf "Set [@,@[<hv 2> %a @]@,]" (list_sep printer (fun ppf () -> fprintf ppf " ;@ ")) lst

let pp_ex_propagator_state = fun ppf (Heuristic_state { plugin = { selector ; propagator ; printer ; printer_json=_ } ; already_selected }) ->
  ignore ( selector, propagator );
  Format.fprintf ppf "{ selector = (* OCaml function *); propagator = (* OCaml function *); already_selected = %a }"
  (pp_already_selected printer) already_selected

let json_already_selected = fun printer_json set : Yojson.Safe.t ->
  let lst = (RedBlackTrees.PolySet.elements set) in
let list f lst = `List (List.map ~f:f lst) in
    `List [`String "Set"; (list printer_json lst)]

let json_ex_propagator_state = fun (Heuristic_state { plugin = { selector; propagator; printer=_ ; printer_json } ; already_selected }) : Yojson.Safe.t ->
  ignore (selector,propagator);
  `Assoc[ ("selector", `String "OCaml function"); ("propagator", `String "OCaml function"); ("already_selected" ,          (json_already_selected printer_json) already_selected)]
