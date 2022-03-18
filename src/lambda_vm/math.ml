open Int64
let ( + ) = add
let ( lsl ) = shift_left
let ( lsr ) = shift_right_logical

let lower_bits x = (x lsl 32) lsr 32
let higher_bits x = x lsr 32
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

  (z, c)

let triple_add_with_carry x y z =
  let l, c1 = add_with_carry x y in
  let l, c2 = add_with_carry l z in
  (l, c1 + c2)

let add_bignum x y =
  let rec add_bignum x y c =
    match (x, y) with
    | x :: xs, y :: ys ->
      let l, c = triple_add_with_carry x y c in
      l :: add_bignum xs ys c
    | ([] as xs), y :: ys ->
      let l, c = add_with_carry y c in
      l :: add_bignum xs ys c
    | x :: xs, ([] as ys) ->
      let l, c = add_with_carry x c in
      l :: add_bignum xs ys c
    | [], [] -> if c <> 0L then [c] else [] in
  add_bignum x y 0L
