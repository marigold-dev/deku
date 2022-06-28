
let assert_msg ((message, condition): (string * bool)) =
if not condition then
  failwith message