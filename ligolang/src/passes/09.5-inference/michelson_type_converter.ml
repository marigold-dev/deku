open Typer_common.Errors
open Ast_core
open Trace

let field_checks ~raise kvl loc =
  let () = Assert.assert_true ~raise 
    (too_small_record loc)
    (List.length kvl >=2) in
  let all_undefined = List.for_all ~f:(fun (_,({decl_pos;_}:row_element)) -> decl_pos = 0) kvl in
  let () = Assert.assert_true ~raise 
    (declaration_order_record loc)
    (not all_undefined) in
  ()

let annotate_field (field: ty_expr row_element_mini_c) (ann:string) : ty_expr row_element_mini_c =
  {field with michelson_annotation=Some ann}


let comb_pair (t:type_content) : ty_expr row_element_mini_c =
  let associated_type = {
    type_content = t ;
    sugar = None ;
    location = Location.generated ; }
  in
  {associated_type ; michelson_annotation = Some "" ; decl_pos = 0}

let comb_ctor (t:type_content) : ty_expr row_element_mini_c =
  let associated_type = {
    type_content = t ;
    sugar = None ;
    location = Location.generated ; } in
  {associated_type ; michelson_annotation = Some "" ; decl_pos = 0}

let rec to_right_comb_pair l new_map =
  match l with
  | [] -> new_map
  | [ (Label ann_l, row_element_l) ; (Label ann_r, row_element_r) ] ->
    LMap.add_bindings [
      (Label "0" , annotate_field row_element_l ann_l) ;
      (Label "1" , annotate_field row_element_r ann_r) ] new_map
  | (Label ann, field)::tl ->
    let new_map' = LMap.add (Label "0") (annotate_field field ann) new_map in
    LMap.add (Label "1") (comb_pair (T_record {fields=(to_right_comb_pair tl new_map');layout= Some default_layout})) new_map'

let rec to_right_comb_variant l new_map =
  match l with
  | [] -> new_map
  | [ (Label ann_l, row_element_l) ; (Label ann_r, row_element_r) ] ->
    LMap.add_bindings [
      (Label "M_left"  , annotate_field row_element_l ann_l) ;
      (Label "M_right" , annotate_field row_element_r ann_r) ] new_map
  | (Label ann, field)::tl ->
    let new_map' = LMap.add (Label "M_left") (annotate_field field ann) new_map in
    LMap.add (Label "M_right") (comb_ctor (T_sum {fields = (to_right_comb_variant tl new_map') ; layout = Some default_layout})) new_map'

let rec to_left_comb_pair' first l new_map =
  match l with
  | [] -> new_map
  | (Label ann_l, row_element_l) :: (Label ann_r, row_element_r) ::tl when first ->
    let new_map' = LMap.add_bindings [
      (Label "0" , annotate_field row_element_l ann_l) ;
      (Label "1" , annotate_field row_element_r ann_r) ] LMap.empty in
    to_left_comb_pair' false tl new_map'
  | (Label ann, field)::tl ->
    let new_map' = LMap.add_bindings [
      (Label "0" , comb_pair (T_record {fields=new_map;layout=Some default_layout})) ;
      (Label "1" , annotate_field field ann ) ;] LMap.empty in
    to_left_comb_pair' first tl new_map'
let to_left_comb_pair = to_left_comb_pair' true

let rec to_left_comb_variant' first l new_map =
  match l with
  | [] -> new_map
  | (Label ann_l, row_element_l) :: (Label ann_r, row_element_r) ::tl when first ->
    let new_map' = LMap.add_bindings [
      (Label "M_left"  , annotate_field row_element_l ann_l) ;
      (Label "M_right" , annotate_field row_element_r ann_r) ] LMap.empty in
    to_left_comb_variant' false tl new_map'
  | (Label ann, ctor)::tl ->
    let new_map' = LMap.add_bindings [
      (Label "M_left"  , comb_ctor (T_sum { fields = new_map ; layout = Some default_layout})) ;
      (Label "M_right" , annotate_field ctor ann ) ;] LMap.empty in
    to_left_comb_variant' first tl new_map'
let to_left_comb_variant = to_left_comb_variant' true

let rec from_right_comb_pair ~raise (l:row_element label_map) (size:int) : row_element list =
  let l' = LMap.to_kv_list l in
  match l' , size with
  | [ (_,l) ; (_,r) ] , 2 -> [ l ; r ]
  | [ (_,l) ; (_,{associated_type=tr;_}) ], _ ->
    let comb_lmap = trace_option ~raise (expected_record Location.generated tr) @@ get_t_record tr in
    let next = from_right_comb_pair ~raise comb_lmap.fields (size-1) in
    (l :: next)
  | _ -> raise.raise (corner_case "Could not convert michelson_pair_right_comb pair to a record")

let rec from_left_comb_pair ~raise (l:row_element label_map) (size:int) : row_element list =
  let l' = LMap.to_kv_list l in
  match l' , size with
  | [ (_,l) ; (_,r) ] , 2 -> [ l ; r ]
  | [ (_,{associated_type=tl;_}) ; (_,r) ], _ ->
    let comb_lmap = trace_option ~raise (expected_record Location.generated tl) @@ get_t_record tl in
    let next = from_left_comb_pair ~raise comb_lmap.fields (size-1) in
    (List.append next [r])
  | _ -> raise.raise (corner_case "Could not convert michelson_pair_left_comb pair to a record")

let rec from_right_comb_variant ~raise (l:row_element label_map) (size:int) : row_element list =
  let l' = LMap.to_kv_list l in
  match l' , size with
  | [ (_,l) ; (_,r) ] , 2 -> [ l ; r ]
  | [ (_,l) ; (_,{associated_type=tr;_}) ], _ ->
    let comb_cmap = trace_option ~raise (expected_variant Location.generated tr) @@ get_t_sum tr in
    let next = from_right_comb_variant ~raise comb_cmap.fields (size-1) in
    (l :: next)
  | _ -> raise.raise (corner_case "Could not convert michelson_or right comb to a variant")

let rec from_left_comb_variant ~raise (l:row_element label_map) (size:int) : row_element list =
  let l' = LMap.to_kv_list l in
  match l' , size with
  | [ (_,l) ; (_,r) ] , 2 -> [ l ; r ]
  | [ (_,{associated_type=tl;_}) ; (_,r) ], _ ->
    let comb_cmap = trace_option ~raise (expected_variant Location.generated tl) @@ get_t_sum tl in
    let next = from_left_comb_variant ~raise comb_cmap.fields (size-1) in
    (List.append next [r])
  | _ -> raise.raise (corner_case "Could not convert michelson_or left comb to a record")

let convert_pair_to_right_comb l =
  let l' = List.sort ~compare:(fun (_,{associated_type=_;decl_pos=a;_}) (_,{associated_type=_;decl_pos=b;_}) -> Int.compare a b) l in
  T_record {fields=(to_right_comb_pair l' LMap.empty);layout=Some default_layout}

let convert_pair_to_left_comb l =
  let l' = List.sort ~compare:(fun (_,{associated_type=_;decl_pos=a;_}) (_,{associated_type=_;decl_pos=b;_}) -> Int.compare a b) l in
  T_record {fields=(to_left_comb_pair l' LMap.empty);layout=Some default_layout}

let convert_pair_from_right_comb ~raise (src:ty_expr row_element_mini_c label_map) (dst:ty_expr row_element_mini_c label_map) : type_content =
  let fields = from_right_comb_pair ~raise src (LMap.cardinal dst) in
  let labels = List.map ~f:(fun (l,_) -> l) @@
    List.sort ~compare:(fun (_,{associated_type=_;decl_pos=a;_}) (_,{associated_type=_;decl_pos=b;_}) -> Int.compare a b ) @@
    LMap.to_kv_list_rev dst in
  T_record {fields=(LMap.of_list @@ List.zip_exn labels fields);layout=Some default_layout}

let convert_pair_from_left_comb ~raise (src:ty_expr row_element_mini_c label_map) (dst:ty_expr row_element_mini_c label_map) : type_content =
  let fields = from_left_comb_pair ~raise src (LMap.cardinal dst) in
  let labels = List.map ~f:(fun (l,_) -> l) @@
    List.sort ~compare:(fun (_,{associated_type=_;decl_pos=a;_}) (_,{associated_type=_;decl_pos=b;_}) -> Int.compare a b ) @@
    LMap.to_kv_list_rev dst in
  T_record {fields=(LMap.of_list @@ List.zip_exn labels fields);layout=Some default_layout}

let convert_variant_to_right_comb l =
  let l' = List.sort ~compare:(fun (_,{associated_type=_;decl_pos=a;_}) (_,{associated_type=_;decl_pos=b;_}) -> Int.compare a b) l in
  T_sum {fields = to_right_comb_variant l' LMap.empty ; layout = Some default_layout }

let convert_variant_to_left_comb l =
  let l' = List.sort ~compare:(fun (_,{associated_type=_;decl_pos=a;_}) (_,{associated_type=_;decl_pos=b;_}) -> Int.compare a b) l in
  T_sum { fields = to_left_comb_variant l' LMap.empty ; layout = Some default_layout }

let convert_variant_from_right_comb ~raise (src:ty_expr row_element_mini_c label_map) (dst:ty_expr row_element_mini_c label_map) : type_content =
  let ctors = from_right_comb_variant ~raise src (LMap.cardinal dst) in
  let ctors_name = List.map ~f:(fun (l,_) -> l) @@
    List.sort ~compare:(fun (_,{associated_type=_;decl_pos=a;_}) (_,{associated_type=_;decl_pos=b;_}) -> Int.compare a b ) @@
    LMap.to_kv_list_rev dst in
  (T_sum { fields = LMap.of_list @@ List.zip_exn ctors_name ctors ; layout = Some default_layout })

let convert_variant_from_left_comb ~raise (src:ty_expr row_element_mini_c label_map) (dst:ty_expr row_element_mini_c label_map) : type_content =
  let ctors = from_left_comb_variant ~raise src (LMap.cardinal dst) in
  let ctors_name = List.map ~f:(fun (l,_) -> l) @@
    List.sort ~compare:(fun (_,{associated_type=_;decl_pos=a;_}) (_,{associated_type=_;decl_pos=b;_}) -> Int.compare a b ) @@
    LMap.to_kv_list_rev dst in
  (T_sum { fields = LMap.of_list @@ List.zip_exn ctors_name ctors ; layout = Some default_layout })
