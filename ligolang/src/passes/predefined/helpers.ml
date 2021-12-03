module Stacking = struct

  open Tezos_utils.Michelson

  type predicate =
    | Simple of unit michelson
    | Special of ((string -> unit michelson) -> unit michelson)

  (* TODO why did these exist? *)
  let simple_constant c = Simple c
  let simple_unary c = Simple c
  let simple_binary c = Simple c
  let simple_ternary c = Simple c
  let simple_tetrary c = Simple c
  let simple_pentary c = Simple c
  let simple_hexary c = Simple c

  (* A special operator which just applies type arguments to a
     Michelson prim. *)
  let trivial_special prim = Special (fun with_args -> with_args prim)
  (* A more special operator which might apply type (or script)
     arguments to some part of its output. *)
  let special c = Special c

  let unpredicate loc apply_args pred =
    Tezos_micheline.Micheline.map_node (fun _ -> loc) (fun p -> p)
      (match pred with
       | Simple code -> code
       | Special code -> code apply_args)
end
