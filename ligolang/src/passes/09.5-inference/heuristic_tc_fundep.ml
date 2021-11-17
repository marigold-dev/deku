(* selector / propagation rule for restricting a type class
     (α₁, α₂, …) ∈ { (τ₁₁, τ₁₂, …) , … }
   to the possible cases, given a second hypothesis of the form
     αᵢ = κ(β₁, β₂, …)
      or
     αᵢ = ρ(ℓᵢ : βᵢ, …)
   It restricts the number of possible cases and replaces αᵢ in
   tuple of constrained variables so that the βⱼ are constrained
   instead.

   This rule can deduce a new assignment for other variables
   constrained by the typeclass if every possible type for that
   variable uses the same type constructor. *)

(* TODO: have a heuristic that restricts typeclass constraints with
   repeated or aliased type variables in the arguments, i.e. of the
   form […;x;…;y;…] ∈ […] where x and y are identical or aliased. *)

open Trace
open Typer_common.Errors
open Simple_utils

module TYPE_VARIABLE_ABSTRACTION = Type_variable_abstraction.TYPE_VARIABLE_ABSTRACTION

module INDEXES = functor (Type_variable : sig type t end) (Type_variable_abstraction : TYPE_VARIABLE_ABSTRACTION(Type_variable).S) -> struct
  module All_plugins = Database_plugins.All_plugins.M(Type_variable)(Type_variable_abstraction)
  open All_plugins
  module type S = sig
    val assignments : Type_variable.t Assignments.t
    val typeclasses_constraining : Type_variable.t Typeclasses_constraining.t
  end
end

module M = functor (Type_variable : sig type t end) (Type_variable_abstraction : TYPE_VARIABLE_ABSTRACTION(Type_variable).S) -> struct
  open Type_variable_abstraction
  open Type_variable_abstraction.Types

  module Utils = Heuristic_tc_fundep_utils.Utils(Type_variable)(Type_variable_abstraction)
  open Utils
  open Utils.All_plugins

  type flds = (module INDEXES(Type_variable)(Type_variable_abstraction).S)

  type selector_output = {
    tc : c_typeclass_simpl ;
    c :  constructor_or_row ;
  }

  let heuristic_name = "tc_fundep"
  
(* ***********************************************************************
 * Selector
 * *********************************************************************** *)

(* selector:
 *   find in db "αᵢ = κ(β…)" and "(…,αᵢ,…) ∈ ∃δ…, c… => [ (…,τᵢⱼ,…) … ]"
 *   find in db "αᵢ = Ξ(ℓ:β…)" and "(…,αᵢ,…) ∈ ∃δ…, c… => [ (τ…) … ]" *)
  
(* Find typeclass constraints in the dbs which constrain c_or_r.tv *)
let selector_by_variable : (type_variable -> type_variable) -> flds -> constructor_or_row -> type_variable -> selector_output list =
  fun repr (module Indexes) c_or_r tv ->
  let typeclasses = Typeclasses_constraining.get_list (repr tv) (module Indexes) in
  List.map ~f:(fun tc -> { tc ; c = c_or_r }) typeclasses

(* Find constructor constraints α = κ(β …) and and row constraints
   α = Ξ(ℓ:β …) where α is one of the variables constrained by the
   typeclass constraint tcs. *)
let selector_by_tc : (type_variable -> type_variable) -> flds -> c_typeclass_simpl -> selector_output list =
  fun repr (module Indexes) tc ->
    (* Format.eprintf "In selector_by_tc : %a\n%!" PP.c_typeclass_simpl_short tc; *)
  let aux tv =
    (* Since we are only refining the typeclass one type expression
       node at a time, we only need the top-level assignment for
       that variable, e.g. α = κ(βᵢ, …). We can therefore look
       directly in the assignments. *)
    (* Format.eprintf "Searching for tv :%a, repr: %a in assignments\n%!" PP.type_variable tv PP.type_variable @@ repr tv; *)
    match Assignments.find_opt (repr tv) Indexes.assignments with
    | Some cr -> 
        [{ tc ; c = cr }]
    | None    -> 
        [] in
  List.concat @@ List.map ~f:aux tc.args

let selector : (type_variable -> type_variable) -> type_constraint_simpl -> flds -> selector_output list =
  fun repr type_constraint_simpl indexes ->
  (* Format.eprintf "in selector for tc_fundep with type_constraint : %a\n%!" PP.type_constraint_simpl_short type_constraint_simpl ; *)
  match type_constraint_simpl with
    SC_Apply        _ -> []
  | SC_Abs          _ -> []
  | SC_Constructor c  -> selector_by_variable repr indexes (`Constructor c) c.tv
  | SC_Row r          -> selector_by_variable repr indexes (`Row         r) r.tv
  | SC_Alias        _  -> [] (* TODO: this case should go away since aliases are handled by the solver structure *)
  | SC_Poly         _  -> []
  | SC_Access_label _  -> []
  | SC_Typeclass   tc -> selector_by_tc repr indexes tc

(* selector:
 *   find in db γᵢ = κ(β…)  and ∃δ…, c… => (…,αᵢ,…) ∈ [ (…,P_variable γᵢ,…) … ]
 *   find in db γᵢ = Ξ(ℓ:β…) and ∃δ…, c… => (…,αᵢ,…) ∈ [ (…,P_variable γᵢ,…) … ] *)

(* When (αᵢ, …) ∈ { (τ, …) , … } and β = κ(δ …) are in the db,
   aliasing α and β should check if they are non-empty, and in that
   case produce a selector_output for all pairs. This will involve a
   lookup to see if α is constrained by a typeclass
   (typeclasses_constraining indexer). Add to this the logic for
   refined_typeclass vs. typeclass. *)

let alias_selector_half : type_variable -> type_variable -> flds -> selector_output list =
  fun a b (module Indexes) ->
  let a_tcs = Typeclasses_constraining.get_list a (module Indexes) in
  match Assignments.find_opt b Indexes.assignments with
  | Some cr -> List.map ~f:(fun tc -> { tc ; c = cr }) a_tcs
  | None   -> []

let alias_selector : type_variable -> type_variable -> flds -> selector_output list =
  fun a b indexes ->
  alias_selector_half a b indexes @ alias_selector_half b a indexes

let get_referenced_constraints ({ tc; c } : selector_output) : type_constraint_simpl list =
  [
    SC_Typeclass tc;
    (match c with `Constructor c -> SC_Constructor c | `Row r -> SC_Row r);
  ]

(* ***********************************************************************
 * Propagator
 * *********************************************************************** *)

let tv : constructor_or_row -> _ = function `Constructor { tv; _ } -> tv | `Row { tv; _ } -> tv

let typeclass_is_empty (tc : c_typeclass_simpl) = List.length tc.tc = 0

(* TODO: we are calling decude_and_clean here. Do we have all the info
   / are we cleaning everywhere ? Also, this cleanup won't be done
   recursively by the deduce and clean; so the cleaning part should be
   moved to the deduce_and_clean heuristic instead (e.g. so that the
   initial clenaup also deduces and cleans based on the nested constraints). *)
let rec restrict_recur ~raise repr c_or_r (tc_c  : type_constraint_simpl) =
  let opt b = if b then Some tc_c else None in
  match tc_c with
    SC_Apply         _ -> Some tc_c (* Always keep variables constrained by an unresolved SC_Apply; when they get resolved the heuristic will be called again and they will be reconsidered. *)
  | SC_Abs           _ -> raise.raise @@ corner_case "kind error: expected a type (kind *) but got a type abstraction (type-level function, kind _ -> _"
  | SC_Constructor  tc_c  ->
    (match c_or_r with
       `Constructor c     -> opt (Compare.constant_tag c.c_tag tc_c.c_tag = 0 && (List.length c.tv_list) = (List.length tc_c.tv_list))
     | `Row         _     -> None)
  | SC_Row          tc_r  ->
    (match c_or_r with
       `Row         r     -> opt (Compare.row_tag r.r_tag tc_r.r_tag = 0 && List.compare Compare.label (LMap.keys r.tv_map) (LMap.keys tc_r.tv_map) = 0)
     | `Constructor _     -> None)
  | SC_Alias        _     -> raise.raise @@ corner_case "alias constraints not yet supported in typeclass constraints"
  | SC_Poly         _     -> raise.raise @@ corner_case "forall in the nested constraints of a typeclass is unsupported"
  | SC_Typeclass    tc_tc -> let restricted_nested = (restrict ~raise repr c_or_r tc_tc) in
                             if typeclass_is_empty restricted_nested
                             then None
                             else Some (SC_Typeclass restricted_nested)
  | SC_Access_label _l    -> (failwith "TODO: access_label in typeclass constraints not supported yet")

and restrict_cell ~raise repr (c : constructor_or_row) (tc : c_typeclass_simpl) (header : type_variable) (cell : type_value) =
  let return ?(tc = tc) b = (tc,b) in
  if Compare.type_variable (repr @@ tv c) (repr header) = 0 then
    match cell.wrap_content with
    (* P_abs -> … *)
    | P_forall   _                 -> raise.raise @@ corner_case "forall in the righ-hand-side of a typeclass is unsupported"
    | P_constant p                 ->
      (match c with
         `Constructor c ->
         (* let _ = Format.asprintf "%a " PP.constant_tag c.c_tag in *)
          return (Compare.constant_tag c.c_tag p.p_ctor_tag = 0 && (List.length c.tv_list) = (List.length p.p_ctor_args))
       | `Row         _ -> return false)
    | P_row      p                 ->
      (match c with
         `Constructor _ -> return false
       | `Row         r -> return (Compare.row_tag r.r_tag p.p_row_tag = 0 && 
                                    (LMap.is_empty p.p_row_args || List.compare Compare.label (LMap.keys r.tv_map) (LMap.keys p.p_row_args) = 0)))
    | P_variable v
      (* TODO: this should be a set, not a list *)
      when List.mem ~equal:Caml.(=) tc.tc_bound v ->
      let updated_tc_constraints = List.map ~f:(restrict_recur ~raise repr c) tc.tc_constraints in
      let all_accept = List.for_all ~f:(function None -> false | Some _ -> true) updated_tc_constraints in
      let restricted_constraints = List.filter_map ~f:(fun x -> x) updated_tc_constraints in
      let tc = { tc with tc_constraints = restricted_constraints ; original_id = Some (tc.id_typeclass_simpl); id_typeclass_simpl = ConstraintIdentifier.fresh () } in
      return ~tc all_accept
    | P_variable _v                -> return true (* Always keep unresolved variables; when they get resolved the heuristic will be called again and they will be kept or eliminated. *)
    | P_apply    _                 -> failwith "P_apply unsupported, TODO soon"
    | P_abs      _                 -> failwith "P_abs unsupported, TODO soon"
    | P_constraint pc              -> raise.raise @@ corner_case @@ Format.asprintf "Kind error: a cell of a typeclass cannot contain a constraint. %a has kind Constraint but a type-level expression with kind * was expected" PP.type_constraint pc.pc
  else
    return true

and restrict_line ~raise repr c tc (`headers, headers, `line, line) : bool * _ =
  let tc,results = List.fold_map2_exn ~f:(restrict_cell ~raise repr c) ~init:tc headers line in
  (List.for_all ~f:(fun x -> x) results,tc)

and restrict ~raise repr c tc =
  filter_lines (restrict_line ~raise repr c) tc

let propagator : (selector_output, typer_error) Type_variable_abstraction.Solver_types.propagator =
  fun ~raise selected repr ->
    (* Format.eprintf "In propagator for tc_fundep for :%a\n" pp_selector_output selected;  *)
  let restricted = restrict ~raise repr selected.c selected.tc in
  let not_changed =
    Compare.(cmp2
      (List.compare type_variable) restricted.args selected.tc.args
      (typeclass)        restricted.tc selected.tc.tc
    ) = 0
  in
  if (not_changed) then
    []
  else
    [{
        remove_constraints = [SC_Typeclass selected.tc];
        add_constraints = [];
        add_constraints_simpl = [SC_Typeclass restricted];
        proof_trace = Axiom (HandWaved "cut with the following (cleaned => removed_typeclass) to show that the removal does not lose info, (removed_typeclass => selected.c => cleaned) to show that the cleaned vesion does not introduce unwanted constraints.")
      }]




(* 

type t = { m : int; n : unit }

type lens_m = int  * { m : _;   n : unit }
type lens_n = unit * { m : int; n : _ }

lens_m (v:t) = t.m, fun new -> { m = new; n = v.n }
lens_n (v:t) = t.n, fun new -> { m = v.m; n = new }

val get_lenses : constructor_or_row -> type_value -> (type_value list * (type_variable list -> constructor_or_row)) option

check shape
list(_) != map(_)
map(_,_,_) != map(_,_,_)
list(_)             != record(f=_,g=_,h=_)
record(f=_,g=_,h=_) != record(f=_,g=_,h=_)

extract
map(int,string,bool)            → [int,string,bool]
record(f=int, g=string, h=bool) → [int,string,bool]

replacement
map(u,i,o)            → [a,b,c] → map(a, b, c)
record(f=u, g=i, h=o) → [a,b,c] → record(f=a, g=b, h=c)
-----------------------------------------------------------
x = map(u,i,o)
(x,y) ∈ [ [ map(int, string, eXist) ; int];
        [ [ list(float)            ; bool] ]

1) delete line list(float) because list(_) != map(_)

x = ctor (inject fresh vars "pointwise")
→ x = map(a, b, c)

return constraint fresh var == arg pointwise
→ return a = int, b = string, c = eXist

simplified constraint
(a,b,c,y) ∈ [ [ int ; string ; eXist ; int] ]
-----------------------------------------------------------
x = record(f=u,g=i,h=o)
(x,y) ∈ [ [ record(f=int, g=string, h=eXist) ; int  ];
        [ [ list(float)                      ; bool ] ]

x = row_ctor (inject fresh vars "pointwise")
→ x = record(f=a, g=b, h=c)

return constraint fresh var == arg pointwise
→ return a = int, b = string, c = bool

simplified constraint
(a,b,c,y) ∈ [ [ int ; string ; eXist ; int] ]





 *)









(* type tag = [ `Constructor of constant_tag | `Row of row_tag ]
 * type 'a comparer = 'a -> 'a -> int *)

(* let get_tag : constructor_or_row -> tag = function
 *     `Constructor c -> `Constructor c.c_tag
 *   | `Row r -> `Row r.r_tag
 * let get_tag' = function P_constant { p_ctor_tag; p_ctor_args } -> ??
 *                       | P_row { p_row_tag; p_row_args } -> ??
 *                       (P_forall _ | P_variable _ | P_apply _ | P_row _ | P_constant _) -> None *)

(* type arg
 * let get_arg : constructor_or_row -> arg = ?? *)

(* module Eq = struct include Ast_core.Compare let (==) fa b = fa b = 0 end *)

(* let restrict_one (cr : constructor_or_row) (allowed : type_value) =
 *   match cr, allowed.wrap_content with
 *   | `Constructor { reason_constr_simpl=_; tv=_; c_tag; tv_list }, P_constant { p_ctor_tag; p_ctor_args } ->
 *     if Compare.constant_tag c_tag p_ctor_tag = 0
 *     then if List.compare_lengths tv_list p_ctor_args = 0
 *       then Some p_ctor_args
 * (\*      then Some (`Constructor p_ctor_args)*\)
 *       else None (\* case removed because type constructors are different *\)
 *     else None   (\* case removed because argument lists are of different lengths *\)
 *   | `Row _, P_row _ -> failwith "TODO: support P_row similarly to P_constant"
 * (\*  | `Row { reason_row_simpl=_; id_row_simpl=_; original_id=_; tv=_; r_tag; tv_map }, P_row { p_row_tag; p_row_args } ->
 *     if Compare.row_tag r_tag p_row_tag = 0
 *     then if List.compare Compare.label (LMap.keys tv_map) (LMap.keys p_row_args) = 0
 *       then Some (`Row p_row_args)
 *       else None (\* case removed because type constructors are different *\)
 *     else None   (\* case removed because argument lists are of different lengths *\)
 * *\)
 *   | _, (P_forall _ | P_variable _ | P_apply _ | P_row _ | P_constant _ | P_abs _ | P_constraint _) -> None (\* TODO: does this mean that we can't satisfy these constraints? *\)
 * 
 * (\* Restricts a typeclass to the possible cases given v = k(a, …) in c *\)
 * let restrict repr (constructor_or_row : constructor_or_row) (tcs : c_typeclass_simpl) =
 *   let (tv_list, tv) = match constructor_or_row with
 *     | `Row r -> List.map ~f:(fun {associated_variable} -> associated_variable) @@ LMap.to_list r.tv_map , (repr r.tv)
 *     | `Constructor c -> c.tv_list , (repr c.tv)
 *   in
 *   let index =
 *     let repr_tv = (repr tv) in
 *     try List.find_index (fun x -> Compare.type_variable repr_tv (repr x) = 0) tcs.args
 *     with Failure _ ->
 *       failwith (Format.asprintf "problem: couldn't find tv = %a in tcs.args = %a"
 *                   PP.type_variable repr_tv (PP_helpers.list_sep_d PP.type_variable) tcs.args);
 *   in
 *   (\* Eliminate the impossible cases and splice in the type arguments
 *      for the possible cases: *\)
 *   let aux allowed_tuple =
 *     splice_or_none (fun allowed -> restrict_one constructor_or_row allowed) index allowed_tuple in
 *   let tc = List.filter_map aux tcs.tc in
 *   (\* Replace the corresponding typeclass argument with the type
 *      variables passed to the type constructor *\)
 *   let args = splice (fun _arg -> tv_list) index tcs.args in
 *   let id_typeclass_simpl = tcs.id_typeclass_simpl in
 *   { tc_bound = [](\*TODO*\); tc_constraints = [](\*TODO*\); reason_typeclass_simpl = tcs.reason_typeclass_simpl; original_id = tcs.original_id; id_typeclass_simpl ; tc ; args }
 * 
 * let propagator : (selector_output, typer_error) Type_variable_abstraction.Solver_types.propagator =
 *   fun selected repr ->
 *   (\* The selector is expected to provide constraints with the shape (α
 *      = κ(β, …)) and to update the private storage to keep track of the
 *      refined typeclass *\)
 *   let () = Format.eprintf "and tv: %a and repr tv :%a \n%!" (PP_helpers.list_sep_d PP.type_variable) selected.tc.args (PP_helpers.list_sep_d PP.type_variable) @@ List.map ~f:repr selected.tc.args in
 *   let restricted = restrict repr selected.c selected.tc in
 *   let () = Format.eprintf "restricted: %a\n!" PP.c_typeclass_simpl_short restricted in
 *   let (deduced , cleaned) = wrapped_deduce_and_clean repr restricted ~original:selected.tc in
 *   let ret = [
 *       {
 *         remove_constraints = [SC_Typeclass selected.tc];
 *         add_constraints = cleaned :: deduced;
 *         proof_trace = Axiom (HandWaved "cut with the following (cleaned => removed_typeclass) to show that the removal does not lose info, (removed_typeclass => selected.c => cleaned) to show that the cleaned vesion does not introduce unwanted constraints.")
 *       }
 *     ] in
 *   ret *)

(* ***********************************************************************
 * Heuristic
 * *********************************************************************** *)
    
let printer ppd (t : selector_output) =
  let open Format in
  let open Type_variable_abstraction.PP in
  let lst = t.tc in
  let a = t.c in fprintf ppd "%a and %a" c_typeclass_simpl_short lst constructor_or_row_short a

let pp_deduce_and_clean_result_short ppf {deduced;cleaned} =
  let open Format in
  let open Type_variable_abstraction.PP in
  fprintf ppf "{@[<hv 2>@ \
              deduced : %a;@ \
              cleaned : %a;@ \
              @]}"
    (PP_helpers.list_sep_d constructor_or_row_short) deduced
    c_typeclass_simpl_short cleaned

let printer_json (t : selector_output) =
  let open Type_variable_abstraction.Yojson in
  let lst = t.tc in
  let a = t.c in 
  `Assoc [
    ("tc",c_typeclass_simpl lst)
    ;("a",constructor_or_row a)]
let comparator { tc=a1; c=a2 } { tc=b1; c=b2 } =
  let open Type_variable_abstraction.Compare in
  c_typeclass_simpl a1 b1 <? fun () -> constructor_or_row a2 b2
end

module MM = M(Solver_types.Type_variable)(Solver_types.Opaque_type_variable)



open Ast_core.Types
open Solver_types

module Compat = struct
  module All_plugins = Database_plugins.All_plugins.M(Solver_types.Type_variable)(Solver_types.Opaque_type_variable)
  open All_plugins
  include MM
  let compat_flds flds : MM.flds = (module struct
    let assignments : type_variable Assignments.t = flds#assignments
    let typeclasses_constraining : type_variable Typeclasses_constraining.t = flds#typeclasses_constraining
  end)
  let selector repr c flds = MM.selector repr c (compat_flds flds)
  let alias_selector a b flds = MM.alias_selector a b (compat_flds flds)
end
let heuristic = Heuristic_plugin Compat.{ heuristic_name; selector; alias_selector; get_referenced_constraints; propagator; printer; printer_json; comparator }

type nonrec deduce_and_clean_result = MM.Utils.deduce_and_clean_result = {
    deduced : constructor_or_row list ;
    cleaned : c_typeclass_simpl ;
    changed : bool ;
  }
let restrict = MM.restrict
let deduce_and_clean = MM.Utils.deduce_and_clean
let pp_deduce_and_clean_result_short = MM.pp_deduce_and_clean_result_short






(*
                          ========NOTES=======

  (* typeclass size(a) = ∃ x, size(x) =>
                         a ∈ [ (int);
                               (pair(t,t));
                               (list(x)) ] (* une typeclass *) *)
  let x = fresh x

  type t = string
  type u = u2

  new info: a = map3(_,_,_)

  let tc_sizearg3 a   =
                       tc "arguments for size"       [x;y;z] (* bound local vars *)
                                                     [ (* nested constraints *)
                                                       c_typeclass_simpl([x], tc_comparable) ;
                                                       c_typeclass_simpl(tc_comparable = [y,x] ∈ [ [map3[unit,   t,      list[string]], bool ],
                                                                                                   [map3[t,      string, set[string] ], int  ],
                                                                                                   [map3[unit,   string, set[bool]   ], bool ],
                                                                                                   [float                             , bool ]
                                                                                                   [float                             , int  ]]) ;
                                                       c_typeclass_simpl(tc_length     = [y,u'']   ∈ ∃ [w],
                                                                                                    [ (w,u' ∈ [ set[x],       u1
                                                                                                                list[string], u2 ] ]
                                                                                                   =>
                                                                                                   [ [map3[unit,   string, w     ], u' ],
                                                                                                     [map3[unit,   float,  w     ], u' ],
                                                                                                     [float                      ], u' ]
                                                       z = t
                                                     ]
                                                     [a,d,u] (* ∈ *) 
                                                     [ [ int;                      bool , u'' ] ;       (* possibilities *)
                                                       [ pair[t,t];                float, u'' ] ;
                                                       [ map3[unit; x;     y],     float, u'' ] ;
                                                       [ map3[unit; x;     float], float, u'' ] ;
                                                       [ map3[unit; float; x],     float, u'' ] ;
                                                       [ map3[unit; unit;  unit],  float, u'' ] ;
                                                       [ P_variable bool,          float, u'' ] ;
                                                       ∃…[ y,                      float, u'' ] ;
                                                       [ z,                        float, u'' ]
                                                     ]

                         tc_comp = a ∈ [ [unit];[cmp2];[cmp3] ]
                         tc_len = a ∈ [ [map(int,int)];[len2] ]
                         a = map[z;w]
                         // z = autre
                         // w = chose




                                                       cleaned:

                                                       c_typeclass_simpl(tc_comparable = [v,x] ∈ [ [unit    , bool ],
                                                                                                   [string  , int  ]]) ;
                                                       y = map3[v,w,z]
                                                       w = string
                                                       z = string

                                                       c_typeclass_simpl(tc_length     = [l,m] ∈ [ [string, string ],
                                                                                                   [float,  float  ]]
                                                       y = map3[k,l,m]
                                                       k = unit

                                                       y = map3[v,w,z]
                                                       y = map3[k,l,m]
                                                       v=k
                                                       w=l
                                                       z=m


selector:
  find in db αᵢ = κ(β…)  and ∃δ…, c… => (…,αᵢ,…) ∈ [ (…,τᵢⱼ,…) … ]
    if τᵢⱼ != (P_variable unbound)
  find in db αᵢ = Ξ(ℓ:β…) and ∃δ…, c… => (…,αᵢ,…) ∈ [ (τ…) … ]
    if τᵢⱼ != (P_variable unbound)

propagator:
  filter col (* = ᵢ *):
    List.filter ~f:(fltr col) (get_lines matrix)
  deduce:
    List.map ~f:deduce1 (get_columns matrix)

deduce1 column:
  if all_equal_root:
    return v = eq

fltr col line:
  match line[col] with:
    P_abs -> …
  | P_forall -> unsupported
  | P_constant/ctor -> true if same k + len(args)
  | P_row/row -> true if same r + keys
  | P_variable (bound) -> filter recursively in the c…
  | P_variable (unbound)
     -> do not touch a column which contains a var (wait for inlining)
  | P_apply -> unsupported



inline_var:
when a var which appears at the root of a column is found by the selector; inline it
inline_var:
when a var which appears at the root of a column is found by the selector; inline it




inline_var:
when a var which appears at the root of a column is found by the selector; inline it



inline:
if there is only one line, and it contains only variables used in a typeclass, then inline it



test:
	 
  def get_allowed(tc_constraint):
    return enumeration of all allowed types a which satisfy tc_constraint(args), i.e a matrix of type_value with len(args) columns and N rows

  def test_main():
    initial_args = a
    initial_typeclass = size(a)
    allowed = get_allowed(initial_typeclass)
    for each type in allowed:
      genealogies([], [], (a, initial_typeclass), [(a, type)])

  # Order of traversal of the tree where parents are always produced before children
  # (i.e. realistic orders of birth in a genealogy tree where the dates are missing and time-travel is not permitted):
  def genalogies(order, all_deduced, (args, constraint), candidates, get_var_from_path):
    if len(nodes) == 0:
      check_end(cleaned, order)
    else:
      for candidate in candidates:
	ctor_or_row = mk_constraint(candidate) # α = κ(βᵢ…)

        # test stuff
        deduced, cleaned = deduce_and_clean(ctor_or_row, constraint, get_var_from_path)
	new_deduced = new_deduced + deduced
        check(constraint, new_deduced, cleaned, filter, order, candidate)
        constraint = cleaned

        # tree stuff
	new_candidates = Set.add_list (Set.remove candidate candidates) children
        new_order = order + [candidate]
        genealogies(new_order, new_deduced, ctor_or_row, new_candidates)

  def check(constraint, all_deduced, cleaned, filter, order_before_candidate, candidate):
    if allowed(constraint) != List.filter ~f:(mk_filter(partial_assignment), allowed):
      print "Error: cleaned constraint is not as simplified as it should be or it is incorrect"

  def mk_filter(all_deduced, ):

  def check_end():
    # sanity check (already enforced by check())
    # the tc should be empty (fully inferred) at the end
    if len(cleaned.bound) == 0 and len(cleaned.tcs) == 0 and len(cleaned.args) == 0 and len(cleaned.possibilities) == 0:
      print "okay " + order
    else:
      print "error " + order

Test: list of type values
     1 simplify
         |
       2 map
     /       \
3 list    5 map
    |      /   \
 4 int  6 str  7 list
                 |
               8 int

map(x, map(string, x))
map(x, map(float,  x))
map(bool, bool)
x ∈ (list(int), bool)

extension(cleaned) == filter(extension(initial_typeclass), sent_info=map(list(_),_))
already_deduced + deduced ≥ sent_info
extension(cleaned) == filter(extension(initial_typeclass), sent_info=to_partial_type(already_deduced + deduced))
ideally: (already_deduced + deduced) is maximal

Propagator args: (c_constructor_simpl or c_row_simpl or ())                               Propagator deduced
                 + c_typeclass_simpl([a], …)

               1 ()                                                                        1 a=map(l,m)
						       							    
           2 a=map(b,c)				                                               2 []		    
            					       							    
 3 b=list(d)       5 c=map(e,f)			             3 [l=list(n),n=int,m=map(o,l)]                         5 []	    
           					       		   					    
  4 d=int        6 e=str  7 f=list(g)		                     4 []                                 6[o=str]       7[]
                				       							    
                        8 g=int			         	                                                 8[]

Deduced DAG

           map
	/     \
	|    map
	|    :  \
	|  (str)|
	|      /
	 \    /
          list
           |
          int






     

                                                     [ [ x;     y ] ;
                                                       [ x;     float ] ;
                                                       [ float; x ] ;
                                                       [ unit;  unit ] ;
						       [ int,   int] (* from y *)
                                                     ]






  ~> c_typeclass_simpl([x], tc_comparable)  (* x ∈ tc_comparable *)
                       tc "arguments for size"       [x;y] (* bound local vars *)
                                                     [ c_typeclass_simpl([y], tc_length)
                                                        ] (* new tcs *)
                                                     [a] (* ∈ *)
                                                     [
						       map[cmp1; map(int,int)]
						       map[cmp1; len2]
						       map[cmp2; map(int,int)]
						       map[cmp2; len2]
						       map[cmp3; map(int,int)]
						       map[cmp3; len2]
						       map[cmp1; float]
						       map[cmp2; float]
						       map[cmp3; float]
						       map[unit;unit]
						       map(int,int);
						       len2;
                                                     ]

  
                       tc "arguments for size"       […union of tvars from tc_co tc_len…] (* bound local vars *)
                                                     […union ……] (* new tcs *)
                                                     [b fresh;c fresh] (* ∈ *)
                                                     [ [ x ; y ] ;
                                                       [ unit; unit ] ;
                                                       [ y ]
                                                     ]

======== END NOTES =========

*)  
