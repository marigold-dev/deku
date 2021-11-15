open Types


let range i j =
  let rec aux i j acc = if i >= j then acc else aux i (j-1) (j-1 :: acc) in
  aux i j []

let label_range i j =
  List.map ~f:(fun i -> Label (string_of_int i)) @@ range i j

let is_tuple_lmap m =
  List.for_all ~f:(fun i -> LMap.mem i m) @@ (label_range 0 (LMap.cardinal m))

let get_pair m =
  match (LMap.find_opt (Label "0") m , LMap.find_opt (Label "1") m) with
  | Some e1, Some e2 -> Some (e1,e2)
  | _ -> None

let tuple_of_record (m: _ LMap.t) =
  let aux i =
    let label = Label (string_of_int i) in
    let opt = LMap.find_opt (label) m in
    Option.bind ~f:(fun opt -> Some ((label,opt),i+1)) opt
  in
  Base.Sequence.to_list @@ Base.Sequence.unfold ~init:0 ~f:aux

let list_of_record_or_tuple (m: _ LMap.t) =
  if (is_tuple_lmap m) then
    List.map ~f:snd @@ tuple_of_record m
  else
    LMap.to_list m

let kv_list_of_record_or_tuple (m: _ LMap.t) =
  if (is_tuple_lmap m) then
    tuple_of_record m
  else
    LMap.to_kv_list m

let rec fold_pattern : ('a -> 'b pattern -> 'a) -> 'a -> 'b pattern -> 'a =
  fun f acc p ->
    let acc' = f acc p in
    match p.wrap_content with
    | P_unit -> acc'
    | P_var _ -> acc'
    | P_list lp -> (
      match lp with
      | Cons (pa,pb) -> fold_pattern f (fold_pattern f acc' pb) pa
      | List lp -> List.fold_left ~f:(fold_pattern f) ~init:acc' lp 
    )
    | P_variant (_,p) -> fold_pattern f acc' p
    | P_tuple lp -> List.fold_left ~f:(fold_pattern f) ~init:acc' lp
    | P_record (_,lp) -> List.fold_left ~f:(fold_pattern f) ~init:acc' lp

let fold_pattern_list f acc l = List.fold_left ~f:(fold_pattern f) ~init:acc l

let rec map_pattern_t : ('a binder -> 'b binder) -> 'a pattern -> 'b pattern =
  fun f p ->
    let self = map_pattern_t f in
    let ret wrap_content = { p with wrap_content } in
    match p.wrap_content with
    | P_unit -> ret P_unit
    | P_var b ->
      let b' = f b in
      ret (P_var b')
    | P_list lp -> (
      let lp =
        match lp with
        | Cons (pa,pb) ->
          let pa = self pa in
          let pb = self pb in
          (Cons (pa, pb) : 'b list_pattern)
        | List lp ->
          let lp = List.map ~f:self lp in
          (List lp : 'b list_pattern)
      in
      ret @@ P_list lp
    )
    | P_variant (l,p) -> (
      let p = self p in
      ret @@ P_variant (l,p)
    )
    | P_tuple lp ->
      let lp = List.map ~f:self lp in
      ret @@ P_tuple lp
    | P_record (x,lp) ->
      let lp = List.map ~f:self lp in
      ret @@ P_record (x,lp)

let var_attribute = { const_or_var = Some `Var }
let const_attribute = { const_or_var = Some `Const }
let empty_attribute = { const_or_var = None }
