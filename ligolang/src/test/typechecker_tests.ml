open Test_helpers
module Core = Typesystem.Core
open Ast_core.Types
open Ast_core.Reasons
open Ast_core.Combinators

module Random_type_generator = struct

  type m = { var : type_variable ; cor_opt : constructor_or_row option }
  let alias var : m = { var ; cor_opt = None }
  let concrete_t var cor : m = { var ; cor_opt = Some cor }
  let make_var i =
    let name = "n"^(string_of_int i) in 
    Var.fresh ~name ()
  let (base_t_1, base_t_2) = make_var (-1) , make_var (-2)
  let pp : Format.formatter -> m -> unit = fun ppf {var ; cor_opt} ->
    match cor_opt with
    | None -> Format.fprintf ppf "Alias %a" Var.pp var
    | Some cor -> Format.fprintf ppf "Concrete %a -> %a" Var.pp var Ast_core.PP.constructor_or_row cor
  let rec make_type ~base_type1 ~base_type2 ~max_depth i depth tmap  : int * m * m list =
    let tvar = make_var i in
    let continue () = make_type ~base_type1 ~base_type2 ~max_depth i 0 tmap in
    let nest j tmap' = make_type ~base_type1 ~base_type2 ~max_depth j (depth+1) tmap' in
    let return j t tmap' = (j , t , t::tmap') in
    if i <= 0 then (i, alias base_t_1, tmap)
    else (
      let n = Random.int 3 in
      match n with
      | _ when depth = max_depth ->
        let t = if (n mod 2) = 0 then alias base_t_1 else alias base_t_2 in
        let i,_,tmap = continue () in
        (i,t,tmap)
      | 0 ->
        let i,nv,tmap = nest (i-1) tmap in
        let cor = make_constructor_or 0 None tvar C_set [ nv.var ] in
        return i (concrete_t tvar cor) tmap
      | 1 -> 
        let i,nv,tmap = nest (i-1) tmap in
        let cor = make_constructor_or 0 None tvar C_list [ nv.var ] in
        return i (concrete_t tvar cor) tmap
      | 2 ->
        let i,l,tmap = nest (i-1) tmap in
        let i,r,tmap = nest (i-1) tmap in
        let cor = make_constructor_or 0 None tvar C_map [ l.var ; r.var ] in
        return i (concrete_t tvar cor) tmap
      | _ -> failwith "fix definition of n above"
    )
  let generate ~raise nb_type max_depth =
    let base_type1 = make_constructor_or 0 None base_t_1 C_int [] in
    let base_type2 = make_constructor_or 0 None base_t_2 C_nat [] in
    let init_map = [
      {var = base_t_1 ; cor_opt = Some base_type1} ;
      {var = base_t_2 ; cor_opt = Some base_type2} ; ]
    in
    let _,_,map = 
      make_type ~base_type1 ~base_type2 ~max_depth
        nb_type
        0
        init_map
    in
    let map = List.rev map in
    let find_assignment_mock : type_variable -> constructor_or_row option  = fun tv ->
      let t_opt = List.find ~f:(fun ({var ;_}:m) -> Var.equal var tv) map in
      match t_opt with
      | None -> failwith "internal: variable should always been known"
      | Some m -> m.cor_opt
    in
    let repr_mock : type_variable -> type_variable = fun tv -> tv in (*no aliases*)
    let all_vars : type_variable list = List.map ~f:(fun ({var ;_}:m) -> var) map in
    let test_checker ~raise constraints_list =
      Trace.trace ~raise (Main_errors.test_tracer "typechecker tests") @@
      (fun ~raise -> Trace.trace ~raise (Main_errors.inference_tracer)
      (Inference.Typecheck.check constraints_list all_vars repr_mock find_assignment_mock))
    in
    let test_checker_neg ~raise lst =
      Trace.Assert.assert_fail ~raise (Main_errors.test_internal "This check should raise.raise") (test_checker lst)
    in
    (map,test_checker ~raise,test_checker_neg ~raise)
  let build_constraint : m list -> type_constraint_simpl list = fun m ->
    List.filter_map ~f:(fun { var=_ ; cor_opt } -> match cor_opt with
      | Some (`Constructor c) -> Some (SC_Constructor c)
      | Some (`Row r) -> Some (SC_Row r)
      | None -> None) m
end

module Small_env_manual_test = struct

  let all_vars = 
    let v name = Var.fresh ~name () in
    List.map ~f:(fun n -> v n) ["a" ; "b" ; "c" ; "d" ; "e" ; "f" ; "g" ; "h" ; "i" ; "j" ; "k"]
  let (a,b,c,d,e,f,g,h,i,j,k) =
    match all_vars with
    | [a;b;c;d;e;f;g;h;i;j;k] -> (a,b,c,d,e,f,g,h,i,j,k)
    | _ -> failwith "bad generated var"
  let repr_mock : type_variable -> type_variable = fun v ->
    match v with
    | a' when Var.equal a' a -> a (* The repr of a is a *)
    | b' when Var.equal b' b -> a (* The repr of b is also a *)
    | g' when Var.equal g' g -> h
    | h' when Var.equal h' h -> h
    | i' when Var.equal i' i -> j
    | j' when Var.equal j' j -> j
    | k' when Var.equal k' k -> k
    | _ -> v                      (* other variables are alone in their equivalence class *)
  let find_assignment_mock : type_variable -> constructor_or_row option = fun v ->
    (* a = map(c,d) , b = a, c = nat, d = mutez, e = map(f, d), f = nat , g = variant | Foo | Bar , j = record {baz goo} , k = c -> c *)
    match v with
    | a' when Var.equal a' a -> Some (make_constructor_or 1 None a C_map [c ; d])
    | b' when Var.equal b' b -> None
    | c' when Var.equal c' c -> Some (make_constructor_or 2 None c C_nat [])
    | d' when Var.equal d' d -> Some (make_constructor_or 3 None d C_mutez [])
    | e' when Var.equal e' e -> Some (make_constructor_or 4 None e C_map [f ; d])
    | f' when Var.equal f' f -> Some (make_constructor_or 5 None f C_nat [])
    | g' when Var.equal g' g -> None
    | h' when Var.equal h' h -> Some (make_row_or 6 None h C_variant [(Label "foo", c) ; (Label "bar", d)]) 
    | h' when Var.equal h' h -> Some (make_row_or 7 None h C_variant [(Label "foo", c) ; (Label "bar", d)]) 
    | i' when Var.equal i' i -> None
    | j' when Var.equal j' j -> Some (make_row_or 8 None j C_record [(Label "baz", b) ; (Label "goo", e)])
    | k' when Var.equal k' k -> Some (make_constructor_or 9 None k C_arrow [h ; c])
    | v -> failwith ("test internal : FIND_ASSIGNENT_MOCK" ^ (Format.asprintf "%a" Var.pp v))
  let test_checker ~raise constraints_list =
    Trace.trace ~raise (Main_errors.test_tracer "typechecker tests") @@
      fun ~raise -> Trace.trace ~raise (Main_errors.inference_tracer) @@
        Inference.Typecheck.check constraints_list all_vars repr_mock find_assignment_mock
  let test_checker_neg ~raise lst =
    Trace.Assert.assert_fail ~raise (Main_errors.test_internal "This check should raise.raise") (test_checker lst)
end

let alias ~raise () =
  let open Small_env_manual_test in
  let al_ok = make_alias a b in
  let al_nok = make_alias a c in
  let () = test_checker ~raise [al_ok] in
  let () = test_checker_neg ~raise [al_nok] in
  ()
  
let constructor ~raise () =
  let open Small_env_manual_test in
  let ctor_ok = make_sc_constructor 1 None a C_map [c ; d] in
  let () = test_checker ~raise [ctor_ok] in
  let ctor_ok2 = make_sc_constructor 2 None b C_map [c ; d] in
  let () = test_checker ~raise [ctor_ok2] in
  let ctor_ok3 = make_sc_constructor 3 None k C_arrow [ g ; c] in
  let () = test_checker ~raise [ctor_ok3] in
  let ctor_nok = make_sc_constructor 4 None a C_list [c] in
  let () = test_checker_neg ~raise [ctor_nok] in
  ()

let row ~raise () =
  let open Small_env_manual_test in
  let row_ok  = make_sc_row 1 None h C_variant [(Label "foo", c) ; (Label "bar", d)] in
  let () = test_checker ~raise [row_ok] in
  let row_ok2 = make_sc_row 2 None j C_record [(Label "baz", b) ; (Label "goo", e)] in
  let () = test_checker ~raise [row_ok2] in
  let row_ok3 = make_sc_row 3 None g C_variant [(Label "foo", c) ; (Label "bar", d)] in
  let () = test_checker ~raise [row_ok3] in
  let row_ok3 = make_sc_row 4 None i C_record [(Label "baz", b) ; (Label "goo", e)] in
  let () = test_checker ~raise [row_ok3] in
  let row_nok = make_sc_row 5 None h C_record [(Label "foo", c) ; (Label "bar", d)] in
  let () = test_checker_neg ~raise [row_nok] in
  ()

let typeclass ~raise () =
  let open Small_env_manual_test in
  let nat = wrap (Todo "test") @@ P_constant { p_ctor_tag = C_nat ; p_ctor_args = [] } in
  let mutez = wrap (Todo "test") @@ P_constant { p_ctor_tag = C_mutez ; p_ctor_args = [] } in
  let args = [c ; d] in
  let tc_ok : typeclass = [[nat ; mutez]; [mutez ; nat]] in
  let tcc_ok = make_sc_typeclass ~bound:[] ~constraints:[] () tc_ok args in
  let () = test_checker ~raise [tcc_ok] in
  let tc_nok : typeclass = [[nat ; nat]; [mutez ; nat]] in
  let tcc_nok = make_sc_typeclass ~bound:[] ~constraints:[] () tc_nok args in
  let () = test_checker_neg ~raise [tcc_nok] in
  ()

let forall ~raise () =
  let open Typesystem.Shorthands in
  let open Small_env_manual_test in
  (* a = map(c,d) , b = a, c = nat, d = mutez, e = map(f, d), f = nat , g = variant | Foo | Bar , j = record {baz goo} *)
  let unwrap (f:type_value) = match f.wrap_content with P_forall x -> x | _ -> failwith "test internal failure" in
  (* let d = wrap (Todo "test") @@ P_variable d in *)
  let tc x = tc "test" ~bound:[] ~constraints:[] () [x] [ [nat] ; [int] ] in
  let map_lhs x = wrap (Todo "test") @@ P_constant { p_ctor_tag = C_map ; p_ctor_args = [ x ; mutez ] } in
  let forall = forall_tc "x" @@ fun x -> [tc x] => map_lhs x in
  let forall_sc = make_sc_poly a (unwrap forall) in
  let () = test_checker ~raise [forall_sc] in
  ()

let test_generator ~raise () =
  let open Random_type_generator in
  (*this only test the types generator*)
  let res1,_,_ = generate ~raise 7 1000 in
  let res2,_,_ = generate ~raise 100 1000 in
  let res3,_,_ = generate ~raise 200 23 in
  let res4,_,_ = generate ~raise 340 5 in
  (* let () =
    let open Simple_utils.PP_helpers in
    let list_sep_return x = list_sep x (tag "@.") in
    Format.printf "XXXXXX length:%d \n @[<v>%a@ @]" (List.length res) (list_sep_return pp) res
  in *)
  let remove_base_type i = i -2 in
  let () = Trace.Assert.assert_true ~raise (Main_errors.test_internal "failing to test the type generator") @@ (
    (remove_base_type @@ List.length res1 = 7) &&
    (remove_base_type @@ List.length res2 = 100) &&
    (remove_base_type @@ List.length res3 = 200) &&
    (remove_base_type @@ List.length res4 = 340) )
  in
  ()
let random_ctors ~raise () =
  let open! Random_type_generator in
  let (map,test_checker,test_checker_neg) = generate ~raise 200 40 in
  ignore test_checker_neg ;
  let () = test_checker (build_constraint map) in
  ()

let main =
  test_suite "Typechecker" @@
    [
      test "single alias" alias ;
      test "single constructor" constructor ;
      test "single typeclass" typeclass ;
      test "single row" row ;
      test "single forall" forall ;
      test "test generator test" test_generator ;
      test "random constructors" random_ctors ;
    ]
