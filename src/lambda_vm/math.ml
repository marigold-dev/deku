open Int64
let add_with_carry, triple_add_with_carry =
  let ( + ) = add in
  let ( lsl ) = shift_left in
  let ( lsr ) = shift_right_logical in

  let lower_bits x = (x lsl 32) lsr 32 in
  let higher_bits x = x lsr 32 in
  let add_with_carry x y =
    let lx = lower_bits x in
    let ly = lower_bits y in
    let lz_lc = lx + ly in
    let lz = lower_bits lz_lc in
    (* lower carry *)
    let lc = higher_bits lz_lc in

    let hx = higher_bits x in
    let hy = higher_bits y in
    let hz_hc = hx + hy + lc in
    let hz = lower_bits hz_hc in
    (* higher carry *)
    let hc = higher_bits hz_hc in

    let z = (hz lsl 32) + lz in
    let c = hc in

    (z, c) in

  let triple_add_with_carry x y z =
    let l, c1 = add_with_carry x y in
    let l, c2 = add_with_carry l z in
    (l, c1 + c2) in
  (add_with_carry, triple_add_with_carry)

let add_bignat x y =
  let rec add_bignat x y c =
    match (x, y) with
    | x :: xs, y :: ys ->
      let l, c = triple_add_with_carry x y c in
      l :: add_bignat xs ys c
    | ([] as xs), y :: ys ->
      let l, c = add_with_carry y c in
      l :: add_bignat xs ys c
    | x :: xs, ([] as ys) ->
      let l, c = add_with_carry x c in
      l :: add_bignat xs ys c
    | [], [] -> if c <> 0L then [c] else [] in
  add_bignat x y 0L

let append_to_limb l (sublimb, nat) =
  if sublimb = 0 then
    (1, [l])
  else if sublimb = 1 && List.hd nat = 0L then
    (1, [l])
  else
    (sublimb + 1, l :: nat)

let rec add_int64_to_bignat' i (y_limb, ys) =
  if y_limb = 1 then
    let y = List.hd ys in
    let l, c = add_with_carry i y in
    let limb, body = append_to_limb l (1, [c]) in
    (limb, body)
  else
    let y, ys =
      match ys with
      | y :: ys -> (y, ys)
      | _ -> failwith "grr" in
    let l, c = add_with_carry i y in
    append_to_limb l (add_int64_to_bignat' c (y_limb - 1, ys))

let add_bignat' (x_limb, xs) (y_limb, ys) =
  let rec add_bignat x_limb xs y_limb ys c : int * t list =
    if x_limb <> 1 && y_limb <> 1 then
      let x = List.hd xs in
      let y = List.hd ys in
      let l, c = triple_add_with_carry x y c in
      append_to_limb l
        (add_bignat (x_limb - 1) (List.tl xs) (y_limb - 1) (List.tl ys) c)
    else if y_limb = 1 && x_limb <> 1 then
      let y = List.hd ys in
      let x = List.hd xs in
      let l, c = triple_add_with_carry x y c in
      append_to_limb l (add_int64_to_bignat' c (x_limb - 1, List.tl xs))
    else if x_limb = 1 && y_limb <> 1 then
      let y = List.hd ys in
      let x = List.hd xs in
      let l, c = triple_add_with_carry x y c in
      append_to_limb l (add_int64_to_bignat' c (y_limb - 1, List.tl ys))
    else
      let x = List.hd xs in
      let y = List.hd ys in
      let l, c = triple_add_with_carry x y c in
      append_to_limb l (1, [c]) in

  add_bignat x_limb xs y_limb ys 0L
