let () = assert (Sys.int_size = 63)

(* TODO: properly implement this, saturation arithmetic *)
type integral = Z.t

let n_integral_encoding = Data_encoding.n

let z_integral_encoding = Data_encoding.z

let of_int t = if t >= 0 then Some (Z.of_int t) else None
