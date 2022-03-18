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
