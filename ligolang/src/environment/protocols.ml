type t = Edo | Hangzhou
let current = Edo

(* this list is used to print the list of protocols in the CLI help *)
let protocols_str : string list = [
  "edo" ; "hangzhou"
]

let protocols_to_variant : string -> t option = fun p ->
	match p with
	| "current" -> Some current
	| "edo" -> Some Edo
  | "hangzhou" -> Some Hangzhou
	| i when not (List.exists ~f:(String.equal i) protocols_str) -> None 
	| _ -> failwith "internal error: forgot to add the protocol string form to the list ?"

let variant_to_string : t -> string = fun s ->
	match s with
	| Edo -> "edo"
  | Hangzhou -> "hangzhou"
