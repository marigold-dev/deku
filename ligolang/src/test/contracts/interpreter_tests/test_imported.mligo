module D = struct
  type t = bool
  type p = { initial : t; final : t }
  let default : p = { initial = true; final = false }
end

let main (p, _ : D.t * D.t) : operation list * D.t =
  ([] : operation list), p

