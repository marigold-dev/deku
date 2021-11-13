(*
Warning with is a wrapper for warning.
It creates a function to add warning to a list and get the list of warning.
Theses function is then pass the function given.
Generally the wrapper will be use as such:
warning_with @@ fun add_warning get_warnings ->
  let result = function_that_emit_warnings ~add_warning in
  let warnings = get_warnings () in
  rest_of_the processing ()
*)
val warning_with : (('a -> unit) -> (unit -> 'a list) -> 'b) -> 'b


(*
try_with is a wrapper for error.
The wrapper define a raise function that raise a local exception to be use in case of error.
Generally the wrapper will be use as such :
let result = try_with f handler
where f is a function that will use the raise function to throw error
and handler is called with the error parameter is the exception was raised
*)
type 't raise = { raise : 'a . 't -> 'a }
val try_with : (raise:'t raise -> 'b) -> ('t -> 'b) -> 'b

(*
Wrap the try_with in a stdlib result = Ok 'value | Error 'error
 *)
val to_stdlib_result : (raise:'error raise -> 'value) -> ('value, 'error) result

(*
Act as a map for the propagated error
*)
val trace : raise:'b raise -> ('a -> 'b) -> (raise:'a raise -> 'c) -> 'c
(* Similar but erase the previous error instead of casting it *)
val trace_strong : raise:'a raise -> 'a -> (raise:'b raise -> 'c) -> 'c

(* Unwrap an option using our own error instead of exception *)
val trace_option : raise:'a raise -> 'a -> 'b option -> 'b

(* Raise error if the option is Some *)
val trace_assert_fail_option : raise:'a raise -> 'a -> 'b option -> unit

(* Unwrap the result, raising the error if needed *)
val from_result  : raise:'b raise -> ('a,'b) result -> 'a

(* Check if the function is not failing *)
val to_bool : (raise:'b raise -> 'a) -> bool

(* Return the evaluation of the functino as Some(res) | None *)
val to_option : (raise:'b raise -> 'a) -> 'a option

(* Run the second function if the first fails *)
val bind_or : raise:'a raise -> (raise:'b raise -> 'c) -> (raise:'a raise -> 'c) -> 'c
val bind_exists : raise:'a raise -> ((raise:'a raise -> 'b) * (raise:'a raise -> 'b) list) -> 'b
val bind_map_or :
  ('a -> 'b) -> ('c -> raise:'d raise -> 'b) -> ('c -> raise:'a raise -> 'b) ->
  'c -> 'b

(*
Assert module, raise exception if the assertion is false
*)
module Assert :
sig
  val assert_fail : raise:'a raise -> 'a -> (raise:'b raise -> 'c) -> unit
  val assert_true : raise:'a raise -> 'a -> bool -> unit
  val assert_list_size : raise:'a raise -> 'a -> 'b list -> int -> unit
  val assert_list_empty : raise:'a raise -> 'a -> 'b list -> unit
  val assert_list_same_size : raise:'a raise -> 'a -> 'b list -> 'c list -> unit
end
