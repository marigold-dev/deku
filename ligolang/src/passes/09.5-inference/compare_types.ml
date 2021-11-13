open Ast_core
open Trace
open Typer_common.Errors

let rec assert_type_expression_eq ~raise (a, b: (type_expression * type_expression)) : unit = match (a.type_content, b.type_content) with
  | T_app {type_operator=ta;arguments=lsta}, T_app {type_operator=tb;arguments=lstb} -> (
    let () = Assert.assert_true ~raise (different_types a b) (Var.equal ta tb) in
    if List.length lsta <> List.length lstb then
      raise.raise @@ different_types a b
    else
      trace ~raise (fun _ -> different_types a b)
      @@ fun ~raise -> List.iter ~f:(assert_type_expression_eq ~raise) (List.zip_exn lsta lstb)
  )
  | T_app _, _ -> raise.raise @@ different_types a b
  | T_sum sa, T_sum sb -> (
      let sa' = LMap.to_kv_list_rev sa.fields in
      let sb' = LMap.to_kv_list_rev sb.fields in
      let aux ((ka, {associated_type=va;_}), (kb, {associated_type=vb;_})) =
        let _ =
          Assert.assert_true ~raise (corner_case "different keys in sum types")
          @@ (ka = kb) in
        assert_type_expression_eq ~raise (va, vb)
      in
      let _ =
        Assert.assert_list_same_size ~raise (different_types a b)
        sa' sb'
      in
      trace_strong ~raise (different_types a b) @@
      fun ~raise:_ -> List.iter ~f:aux (List.zip_exn sa' sb')
    )
  | T_sum _, _ -> raise.raise @@ different_types a b
  | T_record ra, T_record rb
       when Helpers.is_tuple_lmap ra.fields <> Helpers.is_tuple_lmap rb.fields -> (
    raise.raise @@ different_types a b
  )
  | T_record ra, T_record rb -> (
      let sort_lmap r' = List.sort ~compare:(fun (Label a,_) (Label b,_) -> String.compare a b) r' in
      let ra' = sort_lmap @@ LMap.to_kv_list_rev ra.fields in
      let rb' = sort_lmap @@ LMap.to_kv_list_rev rb.fields in
      let aux ((ka, {associated_type=va;_}), (kb, {associated_type=vb;_})) =
        let () =
          trace_strong ~raise (different_types a b) @@
          let Label ka = ka in
          let Label kb = kb in
          Assert.assert_true (different_types a b) (ka = kb) in
        assert_type_expression_eq ~raise (va, vb)
      in
      let () =
        Assert.assert_true ~raise (different_types a b) @@
          Option.equal Misc.layout_eq ra.layout rb.layout in
      let () =
        Assert.assert_list_same_size ~raise (different_types a b) ra' rb' in
      trace_strong ~raise (different_types a b)
      @@ (fun ~raise:_ -> List.iter ~f:aux (List.zip_exn ra' rb'))

    )
  | T_record _, _ -> raise.raise @@ different_types a b
  | T_arrow {type1;type2}, T_arrow {type1=type1';type2=type2'} ->
      let () = assert_type_expression_eq ~raise (type1, type1') in
      let () = assert_type_expression_eq ~raise (type2, type2') in
      ()
  | T_arrow _, _ -> raise.raise @@ different_types a b
  | T_variable x, T_variable y -> let _ = (x = y) in failwith "TODO : we must check that the two types were bound at the same location (even if they have the same name), i.e. use something like De Bruijn indices or a propper graph encoding"
  | T_variable _, _ -> raise.raise @@ different_types a b
  | T_module_accessor {module_name=mna;element=a}, T_module_accessor {module_name=mnb;element=b} when String.equal mna mnb -> (
      let () = assert_type_expression_eq ~raise (a, b) in
      ()
  )
  | T_module_accessor _,_ -> raise.raise @@ different_types a b
  | T_abstraction x, T_abstraction y ->
    let () = assert_type_expression_eq ~raise (x.type_, y.type_) in
    ()
  | T_abstraction _, _ -> raise.raise @@ different_types a b
  | T_for_all _, _ -> raise.raise @@ different_types a b
  | T_singleton _ , _ -> failwith "TODO: mmmh, not sure comparing singleton should happen (?)"

(* No information about what made it fail *)
let type_expression_eq ab =
  Trace.try_with 
    (fun ~raise -> assert_type_expression_eq ~raise ab; true)
    (fun _ -> false)
