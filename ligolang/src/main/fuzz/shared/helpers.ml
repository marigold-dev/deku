
(* Helpers for transforming literals *)

let transform_int =
  let const0 _ = 0 in
  let id n = n in
  let negative n = -n in
  let incr n = n + 1 in
  let pred n = n - 1 in
  let prod n = 2 * n in
  [id; const0; negative; incr; pred; prod]

let transform_nat =
  let const0 _ = 0 in
  let id n = n in
  let incr n = n + 1 in
  let prod n = 2 * n in
  [id; const0; incr; prod]

let transform_string =
  let constn _ = "" in
  let double s = s ^ s in
  let id s = s in
  [id; String.capitalize_ascii; String.uncapitalize_ascii; String.lowercase_ascii; String.uppercase_ascii; constn; double]
