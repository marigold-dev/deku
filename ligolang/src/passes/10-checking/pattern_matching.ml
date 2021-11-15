(**

This implements the pattern_matching compiler of `Peyton-Jones, S.L., The Implementation of Functional Programming Languages`, chapter 5.
By reduction, this algorithm transforms pattern matching expression into (nested) cases expressions.
`Sugar` match expression being 'pattern matching' expression and `Core`/`Typed` being 'case expressions'.

"Product patterns" (e.g. tuple & record) are considered variables, an extra rule (product_rule) handles them

List patterns are treated as the variant type `NIL | Cons of (hd , tl)` would be.
Option patterns are treated as the variant type `Some a | None` would be

**)

module I = Ast_core
module O = Ast_typed

open Trace
open Errors

type matchees = O.expression_variable list
type pattern = I.type_expression I.pattern 
type typed_pattern = pattern * O.type_expression
type equations = (typed_pattern list * (I.expression * O.environment)) list
type type_fun = raise:typer_error raise -> O.environment -> ?tv_opt:O.type_expression -> I.expression -> O.expression
type rest = O.expression_content

let is_var : _ I.pattern -> bool = fun p ->
  match p.wrap_content with
  | P_var _ -> true
  | P_tuple _ -> true
  | P_record _ -> true
  | P_unit -> true
  | _ -> false
let is_product' : _ I.pattern -> bool = fun p ->
  match p.wrap_content with
  | P_tuple _ -> true
  | P_record _ -> true
  | _ -> false

let is_product : equations -> typed_pattern option = fun eqs ->
  List.find_map
    ~f:(fun (pl,_) ->
      match pl with
      | (p,t)::_ -> if is_product' p then Some(p,t) else None 
      | [] -> None
    )
    eqs

let assert_unit_pattern ~raise (loc:Location.t) (tv: O.type_expression) : unit = 
  trace_option ~raise (wrong_type_for_unit_pattern loc tv) @@
    O.assert_type_expression_eq (O.t_unit () , tv)

let corner_case loc = (corner_case ("broken invariant at "^loc))

let assert_body_t ~raise : body_t:O.type_expression option -> Location.t -> O.type_expression -> unit =
  fun ~body_t loc t ->
    match body_t with
    | Some prev_t -> Helpers.assert_type_expression_eq ~raise loc (prev_t,t)
    | None -> ()

let extract_variant_type ~raise : pattern -> O.label -> O.type_expression -> O.type_expression =
  fun p label t ->
  match t.type_content with
  | T_sum rows -> (
    match O.LMap.find_opt label rows.content with
    | Some t -> t.associated_type
    | None -> raise.raise @@ pattern_do_not_conform_type p t
  )
  | O.T_constant { injection ; parameters = [proj_t] } when String.equal (Ligo_string.extract injection) Stage_common.Constant.option_name -> (
    match label with
    | Label "Some" -> proj_t
    | Label "None" -> (O.t_unit ())
    | Label _ -> raise.raise @@ pattern_do_not_conform_type p t
  )
  | O.T_constant { injection ; parameters = [proj_t] } when String.equal (Ligo_string.extract injection) Stage_common.Constant.list_name -> (
    match label with
    | Label "Cons" -> O.make_t_ez_record [("0",proj_t);("1",t)]
    | Label "Nil" -> (O.t_unit ())
    | Label _ -> raise.raise @@ pattern_do_not_conform_type p t
  )
  | _ -> raise.raise @@ pattern_do_not_conform_type p t

let extract_record_type ~raise : pattern -> O.label -> O.type_expression -> O.type_expression =
  fun p label t ->
  match t.type_content with
  | T_record rows -> (
    match O.LMap.find_opt label rows.content with
    | Some t -> t.associated_type
    | None -> raise.raise @@ pattern_do_not_conform_type p t
  )
  | _ -> raise.raise @@ pattern_do_not_conform_type p t

(**
get_matchee_type [ ( [ (p01,t) , .. , (p0n,t0n) ], body0 ) , .. , ( [ (pk1,t) , .. , (pkn,tkn) ], bodyk ) ]
makes sure that the left-most type/patterns pairs of the equations have the same type and return this type.
It also fails if the pattern do not conform to the type (T_sum with P_variant, T_record with P_tuple/P_record ..)
e.g.
  get_matchee_type [ ( [ (p0,t0) , ... ], body0 ) , .. , ( [ (pk,tk) , ... ], bodyk ) ]
  checks:
    - t0 = t1 = .. = tk 
    - conform p0 t0 && conform p1 t1 && conform pk tk
**)
let type_matchee ~raise : equations -> O.type_expression =
  fun eqs ->
    let pt1s = List.map ~f:(fun el -> List.hd_exn @@ fst el) eqs in
    let conforms : typed_pattern -> unit = fun (p,t) ->
      match p.wrap_content , t.type_content with
      | I.P_var _ , _ -> ()
      | I.P_variant (label,_) , O.T_sum sum_type -> (
        if O.LMap.mem label sum_type.content then ()
        else raise.raise @@ pattern_do_not_conform_type p t
      )
      | I.P_variant _ , O.T_constant { injection ; _ } when String.equal (Ligo_string.extract injection) Stage_common.Constant.option_name -> ()
      | P_tuple tupl , O.T_record record_type -> (
        if (List.length tupl) != (O.LMap.cardinal record_type.content) then
          raise.raise @@ pattern_do_not_conform_type p t
        else ()
      )
      | (P_record (lst,_)) , O.T_record record_type -> (
        let compare = O.Compare.label in
        let sorted_p = List.sort ~compare lst in
        let sorted_l = List.sort ~compare (O.LMap.keys record_type.content) in
        let open! List.Or_unequal_lengths in
        let x = List.iter2 sorted_p sorted_l
          ~f:(fun la lb ->
            if O.Compare.label la lb = 0 then ()
            else raise.raise @@ pattern_do_not_conform_type p t
          )
        in
        match x with
        | Ok () -> ()
        | Unequal_lengths -> raise.raise @@ pattern_do_not_conform_type p t
      )
      | I.P_unit , O.T_constant { injection ; _ } when String.equal (Ligo_string.extract injection) Stage_common.Constant.unit_name -> ()
      | I.P_list _ , O.T_constant { injection ; _ } when String.equal (Ligo_string.extract injection) Stage_common.Constant.list_name -> ()
      | _ -> raise.raise @@ pattern_do_not_conform_type p t
    in
    let aux : O.type_expression option -> typed_pattern -> O.type_expression option = fun t_opt (p,t) ->
      let () = conforms (p,t) in
      match t_opt with
      | None -> (Some t)
      | Some t' ->
        let () =
          trace_strong ~raise (match_error ~expected:t ~actual:t' p.location) @@
            Helpers.assert_type_expression_eq Location.generated (t, t')
        in
        t_opt
    in
    let t = List.fold ~f:aux ~init:None pt1s in
    Option.value_exn t

(**
  `substitute_var_in_body to_subst new_var body` replaces variables equal to `to_subst` with variable `new_var` in expression `body`.
  note that `new_var` here is never a user variable (always previously generated by the compiler)
**)
let rec substitute_var_in_body ~raise : I.expression_variable -> O.expression_variable -> I.expression -> I.expression =
  fun to_subst new_var body ->
    let aux ~raise : unit -> I.expression -> bool * unit * I.expression =
      fun () exp ->
        let ret continue exp = (continue,(),exp) in
        match exp.expression_content with
        | I.E_variable var when Var.equal var.wrap_content to_subst.wrap_content -> ret true { exp with expression_content = E_variable new_var }
        | I.E_let_in letin when Var.equal letin.let_binder.var.wrap_content to_subst.wrap_content ->
          let rhs = substitute_var_in_body ~raise to_subst new_var letin.rhs in
          let letin = { letin with rhs } in
          ret false { exp with expression_content = E_let_in letin}
        | I.E_lambda lamb when Var.equal lamb.binder.var.wrap_content to_subst.wrap_content -> ret false exp
        | I.E_matching m -> (
          let matchee = substitute_var_in_body ~raise to_subst new_var m.matchee in
          let aux : bool -> pattern -> bool =
            fun b p ->
              match p.wrap_content with
              | P_var x when Var.equal x.var.wrap_content to_subst.wrap_content -> true
              | _ -> b
          in
          let cases = List.map
            ~f:(fun (case : _ I.match_case) ->
              match Stage_common.Helpers.fold_pattern aux false case.pattern with
              | true -> case
              | false ->
                let body = substitute_var_in_body ~raise to_subst new_var case.body in
                { case with body }
            )
            m.cases
          in
          ret false { exp with expression_content = I.E_matching {matchee ; cases}}
        )
        | _ -> ret true exp
    in
    let ((), res) = Self_ast_core.fold_map_expression ~raise aux () body in
    res

let make_var_pattern : O.expression_variable -> pattern =
  fun var -> Location.wrap @@ O.P_var { var ; ascr = None ; attributes = Stage_common.Helpers.empty_attribute }

let rec partition : ('a -> bool) -> 'a list -> 'a list list =
  fun f lst ->
    let add_inner x ll =
      match ll with
      | hdl::tll -> (x::hdl)::tll
      | _ -> assert false
    in
    match lst with
    | [] -> []
    | [x] -> [[x]]
    | x::x'::tl ->
      if f x = f x' then add_inner x (partition f (x'::tl))
      else [x] :: (partition f (x'::tl))

(**
  groups together equations that begin with the same constructor
**)
let group_equations ~raise : equations -> equations O.label_map =
  fun eqs ->
    let aux : typed_pattern list * (I.expression * O.environment) -> equations O.label_map -> equations O.label_map =
      fun (pl , (body , env)) m ->
        let (phd,t) = List.hd_exn pl in
        let ptl = List.tl_exn pl in
        let upd : O.type_expression -> pattern -> equations option -> equations option =
          fun proj_t pattern kopt ->
            match kopt with
            | Some eqs ->
              let p = (pattern,proj_t) in
              Some (( p::ptl , (body,env))::eqs)
            | None ->
              let p = (pattern,proj_t) in
              Some [ (p::ptl          , (body,env)) ]
        in
        match phd.wrap_content with
        | P_variant (label,p_opt) ->
          let proj_t = extract_variant_type ~raise phd label t in
          O.LMap.update label (upd proj_t p_opt) m
        | P_list (List []) ->
          let label = O.Label "Nil" in
          let proj_t = extract_variant_type ~raise phd label t in
          O.LMap.update label (upd proj_t (Location.wrap O.P_unit)) m
        | P_list (Cons (p_hd,p_tl)) ->
          let label = O.Label "Cons" in
          let pattern = Location.wrap ~loc:(phd.location) @@ I.P_tuple [p_hd;p_tl] in
          let proj_t = extract_variant_type ~raise phd label t in
          O.LMap.update label (upd proj_t pattern) m
        | _ -> raise.raise @@ corner_case __LOC__
    in
    List.fold_right ~f:aux ~init:O.LMap.empty eqs

let rec match_ ~raise : err_loc:Location.t -> type_f:type_fun -> body_t:O.type_expression option -> matchees -> equations -> rest -> O.expression =
  fun ~err_loc ~type_f ~body_t ms eqs def ->
  match ms , eqs with
  | [] , [([],(body,env))] ->
      let body = type_f ~raise ?tv_opt:body_t env body in
      let () = assert_body_t ~raise ~body_t body.location body.type_expression in
      body
  | [] , eqs when List.for_all ~f:(fun (ps,_) -> List.length ps = 0) eqs ->
    raise.raise @@ redundant_pattern err_loc
  | _ ->
    let leq = partition (fun (pl,_) -> is_var (fst @@ List.hd_exn pl)) eqs in
    let aux = fun part_eq (prev_opt:O.expression option) ->
      let r =
        match prev_opt with
        | None -> consvar ~raise ~err_loc ~type_f ~body_t ms part_eq def
        | Some prev -> consvar ~raise ~err_loc ~type_f ~body_t:(Some prev.type_expression) ms part_eq prev.expression_content
      in
      (Some r)
    in
    let r = List.fold_right ~f:aux ~init:None leq in
    Option.value_exn r

and consvar ~raise : err_loc:Location.t -> type_f:type_fun -> body_t:O.type_expression option -> matchees -> equations -> rest -> O.expression =
  fun ~err_loc ~type_f ~body_t ms eqs def ->
  let p1s = List.map ~f:(fun el -> fst @@ List.hd_exn @@ fst el) eqs in
    if List.for_all ~f:is_var p1s then
      let product_opt = is_product eqs in
      var_rule ~raise ~err_loc ~type_f ~body_t product_opt ms eqs def
    else
      ctor_rule ~raise ~err_loc ~type_f ~body_t ms eqs def

and var_rule ~raise : err_loc:Location.t -> type_f:type_fun -> body_t:O.type_expression option -> typed_pattern option -> matchees -> equations -> rest -> O.expression =
  fun ~err_loc ~type_f ~body_t product_opt ms eqs def ->
  match ms with
  | mhd::mtl -> (
    match product_opt with
    | Some shape ->
      product_rule ~raise ~err_loc ~type_f ~body_t shape ms eqs def
    | None ->
      let aux : typed_pattern list * (I.expression * O.environment) -> (typed_pattern list * (I.expression * O.environment)) =
        fun (pl, (body,env)) ->
        match pl with
        | (phd,t)::ptl -> (
          match phd.wrap_content,t with
          | (P_var b, t) ->
            let body' = substitute_var_in_body ~raise b.var mhd body in
            (* Is substitution avoidable ? mhd here can be the result of a tuple/record destructuring *)
            let env' = O.Environment.add_ez_binder mhd t env in
            (ptl , (body',env'))
          | (P_unit, t) ->
            let () = assert_unit_pattern ~raise phd.location t in
            (ptl , (body,env))
          |  _ -> raise.raise @@ corner_case __LOC__
        )
        | [] -> raise.raise @@ corner_case __LOC__
      in
      let eqs' = List.map ~f:aux eqs in
      match_ ~raise ~err_loc ~type_f ~body_t mtl eqs' def
  )
  | [] -> raise.raise @@ corner_case __LOC__

and ctor_rule ~raise : err_loc:Location.t -> type_f:type_fun -> body_t:O.type_expression option -> matchees -> equations -> rest -> O.expression =
  fun ~err_loc ~type_f ~body_t ms eqs def ->
  match ms with
  | mhd::mtl ->
    let matchee_t = type_matchee ~raise eqs in
    let matchee = O.make_e (O.e_variable mhd) matchee_t in
    let eq_map = group_equations ~raise eqs in
      let aux_p : O.label * equations -> O.matching_content_case =
        fun (constructor,eq) ->
          let proj =
            match eq with
            | [(tp,_)] -> (
              let (pattern,t) = List.hd_exn tp in
              match pattern.wrap_content with
              | P_var x -> x.var
              | P_unit ->
                let () = assert_unit_pattern ~raise pattern.location t in
                Location.wrap @@ Var.fresh ~name:"unit_proj" ()
              | _ -> Location.wrap @@ Var.fresh ~name:"ctor_proj" ()
            )
            | _ ->
              Location.wrap @@ Var.fresh ~name:"ctor_proj" ()
          in
          let new_ms = proj::mtl in
          let nested = match_ ~raise ~err_loc ~type_f ~body_t new_ms eq def in
          ({ constructor ; pattern = proj ; body = nested } : O.matching_content_case)
      in
      let aux_m : O.label * O.type_expression -> O.matching_content_case =
        fun (constructor,t) ->
          let proj = Location.wrap @@ Var.fresh ~name:"ctor_proj" () in
          let body = O.make_e def t in
          { constructor ; pattern = proj ; body }
      in
      let grouped_eqs =
        match O.get_t_sum matchee_t with
        | Some rows ->
          let eq_opt_map = O.LMap.mapi (fun label _ -> O.LMap.find_opt label eq_map) rows.content in
          O.LMap.to_kv_list @@ eq_opt_map
        | None -> (
          match O.get_t_option matchee_t with
          | Some _ -> List.map ~f:(fun label -> (label, O.LMap.find_opt label eq_map)) O.[Label "Some"; Label "None"]
          | None -> (
            match O.get_t_list matchee_t with
            | Some _ -> List.map ~f:(fun label -> (label, O.LMap.find_opt label eq_map)) O.[Label "Cons"; Label "Nil"]
            | None -> raise.raise @@ corner_case __LOC__ (* should be caught when typing the matchee *)
          )
        )
      in
      let present = List.filter_map ~f:(fun (c,eq_opt) -> match eq_opt with Some eq -> Some (c,eq) | None -> None) grouped_eqs in
      let present_cases = List.map ~f:aux_p present in
      let body_t =
        let aux t_opt (c:O.matching_content_case) =
          let () = assert_body_t ~raise ~body_t:t_opt c.body.location c.body.type_expression in
          match t_opt with
          | None -> (Some c.body.type_expression)
          | Some _ -> t_opt
        in
        let t = List.fold ~f:aux ~init:body_t present_cases in
        Option.value_exn t
      in
      let missing = List.filter_map ~f:(fun (c,eq_opt) -> match eq_opt with Some _ -> None | None -> Some (c,body_t)) grouped_eqs in
      let missing_cases = List.map ~f:aux_m missing in
      let cases = O.Match_variant { cases = missing_cases @ present_cases ; tv = matchee_t } in
      O.make_e (O.E_matching { matchee ; cases }) body_t
  | [] -> raise.raise @@ corner_case __LOC__

and product_rule ~raise : err_loc:Location.t -> type_f:type_fun -> body_t:O.type_expression option -> typed_pattern -> matchees -> equations -> rest -> O.expression =
  fun ~err_loc ~type_f ~body_t product_shape ms eqs def ->
  match ms with
  | mhd::_ -> (
    let lb =
      let (p,t) = product_shape in
      match (p.wrap_content,t) with
      | P_tuple ps , t ->
        let aux : int -> _ O.pattern_repr Location.wrap -> (O.label * (O.expression_variable * O.type_expression)) =
          fun i proj_pattern ->
            let l = (O.Label (string_of_int i)) in
            let field_t = extract_record_type ~raise p l t in
            let v = match proj_pattern.wrap_content with
              | P_var x -> x.var
              | _ -> Location.wrap ~loc:proj_pattern.location @@ Var.fresh ~name:"tuple_proj" ()
            in
            (l, (v,field_t))
        in
        List.mapi ~f:aux ps
      | P_record (labels,patterns) , t ->
        let aux : (O.label * _ O.pattern_repr Location.wrap)  -> (O.label * (O.expression_variable * O.type_expression)) =
          fun (l,proj_pattern) ->
            let v = match proj_pattern.wrap_content with
              | P_var x -> x.var
              | _ -> Location.wrap ~loc:proj_pattern.location @@ Var.fresh ~name:"record_proj" ()
            in
            let field_t = extract_record_type ~raise p l t in
            (l , (v,field_t))
        in
        List.map ~f:aux (List.zip_exn labels patterns)
      | _ -> raise.raise @@ corner_case __LOC__
    in
    let aux : typed_pattern list * (I.expression * O.environment) -> (typed_pattern list * (I.expression * O.environment)) =
      fun (pl, (body,env)) ->
      match pl with
      | (prod,t)::ptl -> (
        let var_filler = (make_var_pattern (Location.wrap @@ Var.fresh ~name:"_" ()) , t) in
        match prod.wrap_content with
        | P_tuple ps ->
          let aux i p =
            let field_t = extract_record_type ~raise p (O.Label (string_of_int i)) t in
            (p,field_t)
          in
          let tps = List.mapi ~f:aux ps in
          (tps @ var_filler::ptl , (body,env))
        | P_record (labels,ps) ->
          let aux label p =
            let field_t = extract_record_type ~raise p label t in
            (p,field_t)
          in
          let tps = List.map2_exn ~f:aux labels ps in
          (tps @ var_filler::ptl , (body,env))
        | P_var _ ->
          let filler =
            let (p,t) = product_shape in
            match (p.wrap_content,t) with
            | P_tuple ps , t ->
              let aux i p =
                let field_t = extract_record_type ~raise p (O.Label (string_of_int i)) t in
                let v = match p.wrap_content with
                  | P_var _ -> p
                  | _ -> make_var_pattern (Location.wrap ~loc:p.location @@ Var.fresh ~name:"_" ())
                in
                (v,field_t)
              in
              List.mapi ~f:aux ps
            | P_record (labels,patterns) , t ->
              let aux l p =
                let field_t = extract_record_type ~raise p l t in
                let v = match p.wrap_content with
                  | P_var _ -> p
                  | _ -> make_var_pattern (Location.wrap ~loc:p.location @@ Var.fresh ~name:"_" ())
                in
                (v,field_t)
              in
              List.map2_exn ~f:aux labels patterns
            | _ -> raise.raise @@ corner_case __LOC__
          in
          (filler @ pl , (body,env))
        | _ -> raise.raise @@ corner_case __LOC__
      )
      | [] -> raise.raise @@ corner_case __LOC__
    in
    let matchee_t = type_matchee ~raise eqs in
    let eqs' = List.map ~f:aux eqs in
    let fields = O.LMap.of_list lb in
    let new_matchees = List.map ~f:(fun (_,((x:O.expression_variable),_)) -> x) lb in
    let body = match_ ~raise ~err_loc ~type_f ~body_t (new_matchees @ ms) eqs' def in
    let cases = O.Match_record { fields; body ; tv = snd product_shape } in
    let matchee = O.make_e (O.e_variable mhd) matchee_t in
    O.make_e (O.E_matching { matchee ; cases }) body.type_expression
  )
  | [] -> raise.raise @@ corner_case __LOC__

and compile_matching ~raise ~err_loc ~type_f ~body_t matchee (eqs:equations) =
  let missing_case_default =
    let fs = O.make_e (O.E_literal (O.Literal_string Stage_common.Backends.fw_partial_match)) (O.t_string ()) in
    O.e_failwith fs
  in
  match_ ~raise ~err_loc ~type_f ~body_t [matchee] eqs missing_case_default
