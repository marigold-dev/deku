module Uint128 = struct
  include Stdint.Uint128
  let of_int64 x = x |> Stdint.Uint64.of_int64 |> Stdint.Uint128.of_uint64
  let to_int64 x = x |> Stdint.Uint128.to_uint64 |> Stdint.Uint64.to_int64
  let big_part x = shift_right x 64 |> to_int64
  let small_part x = shift_right (shift_left x 64) 64 |> to_int64
end

let add_with_carry left right =
  let open Uint128 in
  let left = of_int64 left in
  let right = of_int64 right in
  let sum = left + right in
  (small_part sum, big_part sum)
