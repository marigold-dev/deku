exception Out_of_stack

type error = [`Out_of_stack] [@@deriving show]

(* WHY: This is used to avoid stack overflows.

     In the future if OCaml stack overflow detection becomes reliable then
   we can remove it and let it overflow, but for now, manual bound checking.

     About this number, OCaml currently only uses the stack for function calls
   which means each entry is a word and maybe on ARM64 two words,
   in 64bits systems that is 8-16 bytes. The system that has the smallest stack
   is macOS and iOS ARM64 where the secondary stack is about 512kb.

     So by assuming the worst case scenario, that allows us up to 32768, minus
   the stack used by Deku itself. To be conservative as we may have deep
   primitives in the future, I decided to go with two thirds of this, and
   rounding down, which means 20k.
*)
(* TODO: verify properly all this hypothesis and add references to it *)
let max_stack_depth = 20000

let check_stack ~stack = if stack <= 0 then raise Out_of_stack
