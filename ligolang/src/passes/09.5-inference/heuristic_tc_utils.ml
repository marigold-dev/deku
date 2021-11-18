[@@@warning "-32"]
module TYPE_VARIABLE_ABSTRACTION = Type_variable_abstraction.TYPE_VARIABLE_ABSTRACTION

open Trace
open Typer_common.Errors
module Map = RedBlackTrees.PolyMap
module Set = RedBlackTrees.PolySet

module Utils = functor (Type_variable : sig type t end) (Type_variable_abstraction : TYPE_VARIABLE_ABSTRACTION(Type_variable).S) -> struct
  open Type_variable_abstraction
  open Type_variable_abstraction.Types
  type type_variable = Type_variable.t

  module All_plugins = Database_plugins.All_plugins.M(Type_variable)(Type_variable_abstraction)
  (* open All_plugins *)

  let get_cells (tc : c_typeclass_simpl) =
    List.concat tc.tc

  let fold_map_cells (f : 'acc -> type_value -> ('acc * type_value)) (acc : 'acc) (tc : c_typeclass_simpl) =
    let acc, tc_tc = List.fold_map ~f:(fun init l -> List.fold_map ~f ~init l) ~init:acc tc.tc in
    acc, { tc with tc = tc_tc }

  type column = (type_variable * type_value list)
  type columns = column list
  let loop3 : 'e 'x 'a 'b 'c . ('x -> 'a * 'b * 'c) -> ('a * 'b * 'c) -> (('a -> 'a -> 'a) * ('b -> 'b -> 'b) * ('c -> 'c -> 'c)) -> 'x list -> ('a * 'b * 'c) =
    fun f (a0, b0, c0) (a,b,c) xs ->
    let r = List.map ~f:f xs in
    let (as_, bs, cs) = List.unzip3 r in
    (List.fold_left ~f:a ~init:a0 as_, List.fold_left ~f:b ~init:b0 bs, List.fold_left ~f:c ~init:c0 cs)

   let rec transpose_list_of_lists (matrix : type_value list list) =
      match matrix with
        [] -> []
      | (_::_)::_ -> (List.map ~f:List.hd_exn matrix) :: transpose_list_of_lists (List.map ~f:List.tl_exn matrix)
      | []::_ -> assert (List.for_all ~f:List.is_empty matrix); []

  type 'a all_equal = Empty | All_equal_to of 'a | Different

  let rec get_columns (headers : type_variable list) (matrix : type_value list list) =
    match headers with
      [] -> []
    | hd::tl -> (hd, (List.map ~f:List.hd_exn matrix)) :: get_columns tl (List.map ~f:List.tl_exn matrix)

  let rec columns_to_lines (columns : columns) : (type_variable list * type_value list list) =
    match columns with
      [] -> [], []
    | (_header, _::_)::_ ->
      let (headers, matrix) = columns_to_lines @@ List.map ~f:(fun (header, cells) -> header, List.tl_exn cells) columns in
      headers, (List.map ~f:(fun (_header,cells) -> List.hd_exn cells) columns :: matrix)
    | _ -> assert (List.for_all ~f:List.is_empty @@ List.map ~f:snd columns); (List.map ~f:fst columns), []

  let update_columns3 : (columns -> column Rope.SimpleRope.t * 'b * 'c) -> c_typeclass_simpl -> c_typeclass_simpl * 'b * 'c =
    fun f tc ->
    (*let transpose_back cs = let (hs, m) = transpose_back cs in (hs, List.rev m) in*)
    let updated, b, c = f @@ get_columns tc.args tc.tc in
    let headers', matrix' = columns_to_lines @@ Rope.SimpleRope.list_of_rope updated in
    ({ tc with args = headers'; tc = matrix' }, b, c)
    

  let filter_lines (f : _ -> ([`headers] * type_variable list * [`line] * type_value list) -> bool * c_typeclass_simpl) (tc_org : c_typeclass_simpl) =
    let (updated,tc_for_nested) =
      List.fold ~f:(fun (acc,tc) line ->
          let b,tc = f tc (`headers, tc_org.args, `line, line) in
          if b then (line :: acc,tc) else (acc,tc)) ~init:([],tc_org) tc_org.tc
    in
    { tc_org with tc = List.rev updated; tc_constraints = tc_for_nested.tc_constraints }

  (* Check that the typeclass is a rectangular matrix, with one column
    per argument. *)
  let check_typeclass_rectangular ~raise ({ reason_typeclass_simpl=_; tc; args } as tcs : c_typeclass_simpl) =
    let nargs = List.length args in
    if (List.for_all ~f:(fun allowed -> List.length allowed = nargs) tc)
    then tcs
    else raise.raise typeclass_not_a_rectangular_matrix

  (* Check that the transposed typeclass is a rectangular matrix, with
    one row per argument. *)
  let check_typeclass_transposed_rectangular ~raise (tc : (type_variable * type_value list) list) =
    match tc with
      [] -> tc
    | (_, hd) :: tl ->
      let hdlen = List.length hd in
      if List.for_all ~f:(fun (_, l) -> List.length l = hdlen) tl
      then tc
      else raise.raise typeclass_not_a_rectangular_matrix

  let get_tag_and_args_of_constant (tv : type_value) =
    match tv.wrap_content with
    | P_constant { p_ctor_tag; p_ctor_args } -> (p_ctor_tag, p_ctor_args)
    | P_row { p_row_tag; p_row_args } -> ignore (p_row_tag, p_row_args); failwith "TODO: return p_row_tag, p_row_args similarly to P_constant"
    | P_forall _ ->
      (* In this case we would need to do specialization.
        For now we just leave as-is and don't deduce anything *)
      failwith "Unsuported"
    | P_variable _ ->
      (* In this case we  *)
      failwith "TODO : P_variable"
    | P_apply _ ->
      (* In this case we would need to do β-reduction, if
        possible, or invoke another heuristic.
        For now we just leave as-is and don't deduce anything *)
      failwith "TODO P_aply tc_utils"
    | P_abs _ ->
      failwith "TODO P_abs tc_utils"
    | P_constraint _ ->
      failwith "TODO P_constraint tc_utils"
  
let all_equal' : type_constraint_simpl list -> (type_variable -> type_variable) -> type_variable -> type_value list -> (constructor_or_row * type_variable list * type_value list list) all_equal =
  fun tc_constraints repr x type_values ->
    (* Format.eprintf "In all_equal' with tc_constraints : %a, x: %a, type_values:%a\n%!"
      (PP_helpers.list_sep_d PP.type_constraint_simpl_short) tc_constraints
      PP.type_variable x
      (PP_helpers.list_sep_d PP.type_value_short) type_values
     ; *)
    let rec simplify type_values : [> `Constructor of constant_tag * int * type_value_ Location.wrap list | `Row of row_tag * label list * row_value list | `TODO ] list =
      let res = List.map ~f:(
        function
        | P_forall _ -> failwith "forall in typeclass cells is not supported"
        | P_variable v ->
          (* This is a very ad-hoc way to handle nested constraints.
              Instead we should extract the constraints to the outer solver
              (beware: recursion, propagating deductions made under assumptions, other stuff) *)
          let nested_allowed = List.concat @@ List.map
          ~f:(function
              SC_Apply _ -> []
            | SC_Abs   _ -> []
            | SC_Constructor {tv;c_tag;tv_list;_} ->
              if Compare.type_variable (repr tv) (repr v) = 0 then
                [(`Constructor (c_tag, List.length tv_list, List.map ~f:(fun arg -> Location.wrap @@ P_variable arg) tv_list))]
              else
                []
            | SC_Row         {tv;r_tag;tv_map} ->
              if Compare.type_variable (repr tv) (repr v) = 0 then
                [(`Row (r_tag, LMap.keys tv_map, List.map ~f:(fun {associated_variable;michelson_annotation;decl_pos} -> { associated_value = Location.wrap @@ P_variable associated_variable;michelson_annotation;decl_pos}) @@ LMap.values tv_map))]
              else
                []
            | SC_Alias       _ -> failwith "impossible, sc_alias should be removed"
            | SC_Poly        _ -> failwith "polymorphic types cannot be used in typeclass nested constraints"
            | SC_Typeclass nested ->
              (* Get the columns of the nested typeclass constraint which talk about variable v *)
              List.concat @@ List.map
                  ~f:(fun (nested_arg, nested_column) ->
                    if Compare.type_variable (repr nested_arg) (repr v) = 0 then
                      (simplify nested_column)
                    else
                      [])
                  @@ (get_columns nested.args nested.tc)
            | SC_Access_label _ -> [] (* TODO *))
          tc_constraints
          in if List.length nested_allowed = 0 then [`Any] else nested_allowed
        | P_apply      _ -> [`TODO]
        | P_abs        _ -> [`TODO]
        | P_constraint _ -> failwith "kinding error: constraints have kind Constraint, but the cells of a typeclass can only contain types (i.e. kind *)"
        | P_constant { p_ctor_tag; p_ctor_args } -> [`Constructor (p_ctor_tag, List.length p_ctor_args, p_ctor_args)]
        | P_row      { p_row_tag;  p_row_args  } -> [`Row         (p_row_tag, LMap.keys p_row_args, LMap.values p_row_args)]
      ) @@ List.map ~f:(fun x -> x.Location.wrap_content) type_values
      in List.concat res
    in
    let type_values' = simplify type_values in
    (* Format.eprintf "Testing for any\n%!"; *)
    if (List.exists ~f:(function `Any -> true | _ -> false) type_values')
    then Different
    else
    let type_values'' =
      List.filter_map
        ~f:(function `Any -> failwith "impossible, already checked above" | (`TODO | `Constructor _ | `Row _) as x -> Some x)
        type_values' in
    match type_values'' with
      [] -> Empty
    | hd::tl ->
      match hd with
        `TODO -> Different
      | `Constructor (hd_ctor_tag, hd_ctor_length, hd_ctor_args) ->
        if List.for_all ~f:(function `Constructor (p_ctor_tag, p_ctor_length, _p_ctor_args)
              -> Compare.constant_tag p_ctor_tag hd_ctor_tag = 0 && Int.compare p_ctor_length hd_ctor_length == 0 | _ -> false) tl
        then
          let fresh_vars = List.map ~f:(fun _arg -> Core.fresh_type_variable ()) hd_ctor_args in
          let deduced : c_constructor_simpl = {
            id_constructor_simpl = ConstraintIdentifier.fresh ();
            original_id = None;
            reason_constr_simpl = "inferred because it is the only remaining possibility at this point according to the typeclass [TODO:link to the typeclass here]" ;
            tv = (repr x);
            c_tag = hd_ctor_tag;
            tv_list = fresh_vars
          } in
          All_equal_to (`Constructor deduced, fresh_vars,
             List.map ~f:(function `Constructor (_p_ctor_tag, _p_ctor_length, p_ctor_args) -> p_ctor_args | _ -> failwith "impossible") (hd :: tl))
        else Different
      | (`Row (hd_row_tag, hd_row_keys, hd_row_values)) as hd ->
        if List.for_all ~f:(function `Row (p_row_tag, p_row_keys, _p_row_values) ->
                           Compare.row_tag p_row_tag hd_row_tag = 0
                           && List.compare Compare.label hd_row_keys p_row_keys = 0
                         | _ -> false)
                        tl
        then
          let deduced : c_row_simpl = {
            id_row_simpl = ConstraintIdentifier.fresh ();
            original_id = None;
            reason_row_simpl = "inferred because it is the only remaining possibility at this point according to the typeclass [TODO:link to the typeclass here]" ;
            tv = (repr x);
            r_tag = hd_row_tag;
            tv_map = LMap.of_list @@ List.map
              ~f:(fun (k,({ associated_value=_; michelson_annotation; decl_pos } : row_value)) ->
                (k, ({ associated_variable=Core.fresh_type_variable (); michelson_annotation; decl_pos} : row_variable)))
              (List.zip_exn hd_row_keys hd_row_values)
          } in
          let fresh_vars = List.map ~f:(fun x -> x.associated_variable) @@ LMap.values deduced.tv_map in
          All_equal_to (`Row deduced, fresh_vars,
             List.map
               ~f:(function `Row (_p_row_tag, _p_row_keys, p_row_values) ->
                  List.map ~f:(fun x -> x.associated_value) @@ p_row_values
                | _ -> failwith "impossible")
             (hd :: tl))
        else Different
      

(* input:
     x ? [ map3( nat , unit , float ) ; map3( bytes , mutez , float ) ]
   output:
     true,
     [ x = map( m , n , o ) ; o = float ( ) ],
     [ m ? [ nat  ; bytes ]
       n ? [ unit ; mutez ] ]
   input:
     x ? [ record( a = nat , b = unit , c = float ) ; record( a = bytes , b = mutez , c = float ) ]
   output:
     true,
     [ x = record( a=m , b=n , c=o ) ; o = float ( ) ],
     [ m ? [ nat  ; bytes ]
       n ? [ unit ; mutez ] ] *)
let rec replace_var_and_possibilities_1
    ~raise
    tc_constraints
    (repr:type_variable -> type_variable)
    ((x : type_variable), (possibilities_for_x : type_value list))
    : column Rope.SimpleRope.t * _ * bool =
  let open Rope.SimpleRope in
  (*let tags_and_args = List.map ~f:get_tag_and_args_of_constant possibilities_for_x in
  let tags_of_constructors, arguments_of_constructors = List.unzip tags_and_args in*)
  match all_equal' tc_constraints repr x possibilities_for_x  with
  | Different ->
    (* The "changed" boolean return indicates whether any update was done.
       It is used to detect when the variable doesn't need any further cleanup. *)
    (singleton (x, possibilities_for_x), empty, false)            (* Leave as-is, don't deduce anything *)
  | Empty ->
    (* TODO: keep track of the constraints used to refine the
       typeclass so far. *)
    (* fail @@ typeclass_error
     *   "original expected by typeclass"
     *   "<actual> partially guessed so far (needs a recursive substitution)" *)
    (* TODO: possible bug: if there is nothing left because everything was inferred, we shouldn't fail and just continue with an empty TC… can this happen? *)
    raise.raise @@ corner_case "type error: the typeclass does not allow any type for \
                         the variable %a:PP_variable:x at this point"
  | All_equal_to (deduced, fresh_vars, arguments_of_constructors) ->
      (* discard the identical tags, splice their arguments instead, and deduce the x = tag(…) constraint *)

      let (rec_cleaned, rec_deduced, _rec_changed) =
        replace_var_and_possibilities_rec ~raise tc_constraints repr (List.zip_exn fresh_vars (transpose_list_of_lists arguments_of_constructors))
      in
      (* The "changed" boolean return indicates whether any update was done.
         It is used to prevent removal + update of the typeclass if it wasn't modified. *)
      (rec_cleaned, pair (singleton deduced) rec_deduced, true)

  and replace_var_and_possibilities_rec ~raise tc_constraints repr matrix =
    (* Format.eprintf "tc_constraints : %a; matrix: %a\n%!"
      (PP_helpers.list_sep_d PP.type_constraint_simpl_short) tc_constraints
      PP.(PP_helpers.list_sep_d (fun ppf (a,b) -> 
        Format.fprintf ppf "%a,(%a)" type_variable a (PP_helpers.list_sep_d type_value_short) b)) matrix
    ;*)
    let open Rope.SimpleRope in
    (loop3 (replace_var_and_possibilities_1 ~raise tc_constraints repr) (empty, empty, false) (pair, pair, (||))) matrix

type deduce_and_clean_result = {
  deduced : constructor_or_row list ;
  cleaned : c_typeclass_simpl ;
  changed : bool
}

let rec deduce_and_clean_constraints ~raise repr (c : type_constraint_simpl) =
  match c with
  | SC_Typeclass tc ->
   let {cleaned;deduced;changed} = deduce_and_clean ~raise repr tc in
      ((SC_Typeclass cleaned) :: (List.map ~f:(function `Constructor c -> SC_Constructor c | `Row r -> SC_Row r) deduced), changed)
  | other -> ([other], false)

and deduce_and_clean ~raise : (_ -> _) -> c_typeclass_simpl -> deduce_and_clean_result = fun repr tcs ->
  let open Rope.SimpleRope in
  (* Format.eprintf "In deduce_and_clean for : %a\n%!" PP.c_typeclass_simpl_short tcs; *)
  (* ex.   [ x                             ; z      ]
       ∈ [ [ map3( nat   , unit  , float ) ; int    ] ;
           [ map3( bytes , mutez , float ) ; string ] ] *)

  let deduced_and_cleaned_nested_constraints = List.map ~f:(deduce_and_clean_constraints ~raise repr) tcs.tc_constraints in
  let deduced_and_cleaned_nested_constraints', changed' = List.unzip deduced_and_cleaned_nested_constraints in
  let tcs' = { tcs with tc_constraints = List.concat deduced_and_cleaned_nested_constraints' } in

  let (cleaned, deduced, changed) = update_columns3 (replace_var_and_possibilities_rec ~raise tcs.tc_constraints repr) tcs' in
  let changed'' = changed || List.exists ~f:(fun x -> x) changed' in
  (* ex. cleaned:
           [ fresh_x_1 ; fresh_x_2 ; y      ]
       ∈ [ [ nat       ; unit      ; int    ]
           [ bytes     ; mutez     ; string ] ]
         deduced:
         [ x         = map3  ( fresh_x_1 , fresh_x_2 , fresh_x_3 ) ;
           fresh_x_3 = float (                                   ) ; ] *)
  let deduced = 
    (* TODO: this is a placeholder, we need some row variables to allow constraints on all the fields of a record https://gitlab.com/ligolang/ligo/-/merge_requests/1189 *)
    List.filter ~f:(function `Row {tv_map} when LMap.is_empty tv_map -> false | _ -> true) @@
    list_of_rope deduced in

  { deduced ; cleaned ; changed = changed'' }

 let wrapped_deduce_and_clean ~raise repr tc ~(original:c_typeclass_simpl) =
  let {deduced; cleaned; changed} = deduce_and_clean ~raise repr tc in
  (* Format.eprintf "retourning with deduce: %a; cleaned: %a; changed: %b\n" 
    (PP_helpers.list_sep_d PP.constructor_or_row_short) deduced
    PP.c_typeclass_simpl_short cleaned
    changed
    ; *)
  let cleaned = SC_Typeclass { cleaned with original_id = Some original.id_typeclass_simpl; id_typeclass_simpl = ConstraintIdentifier.fresh ()} in
  let aux : constructor_or_row -> type_constraint_simpl = function
      `Constructor x -> SC_Constructor x
    | `Row x -> SC_Row x in
  let deduced = List.map ~f:aux deduced in
  (deduced, cleaned, changed)

end
