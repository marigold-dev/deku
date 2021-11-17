(* Serializer/Derserializer *)

type 'a t = {
  name : string ;
  counter : int option ;
}

let to_yojson f =
  match f.counter with
    Some i ->
    `Assoc [
        ("name", `String f.name) ;
        ("counter",  `Int i) ;
      ]
  | None ->
    `Assoc [
        ("name", `String f.name) ;
      ]

let of_yojson = fun t ->
  match t with
  | `Assoc [
      ("name", `String name) ;
      ("counter", `Int i)] ->
      Ok {name; counter = Some i}
  | `Assoc [
      ("name", `String name)] ->
      Ok {name; counter = None}
  | _ ->
     Utils.error_yojson_format "{name: string; counter: int option}"

type names_for_print = { get_name_for_print : 'a . 'a t -> string }
let global_mutable_names_for_print : names_for_print ref =
  ref { get_name_for_print =  (fun _ -> "") }

let with_names_for_print : names_for_print -> (unit -> unit) -> unit = fun names_for_print thunk ->
  let old = !global_mutable_names_for_print in
  let () = global_mutable_names_for_print := names_for_print in
  let () = thunk () in
  let () = global_mutable_names_for_print := old in
  ()

let rec int_to_unicode (x : Int.t) =
  let digit =
    let ( - ) = Int.sub in
    let ( / ) = Int.div in
    let ( * ) = Int.mul in
    match (x - ((x / 10) * 10)) with
      a when Int.equal a 0 -> "₀"
    | a when Int.equal a 1 -> "₁"
    | a when Int.equal a 2 -> "₂"
    | a when Int.equal a 3 -> "₃"
    | a when Int.equal a 4 -> "₄"
    | a when Int.equal a 5 -> "₅"
    | a when Int.equal a 6 -> "₆"
    | a when Int.equal a 7 -> "₇"
    | a when Int.equal a 8 -> "₈"
    | a when Int.equal a 9 -> "₉"
    | _ -> failwith (Format.asprintf "internal error: couldn't pretty-print int64: %d (is it a negative number?)" x)
  in
  if x = 0 then "" else (int_to_unicode (Int.div x 10)) ^ digit

let pp ppf v =
  match v.name, v.counter with
  | "", None -> Format.fprintf ppf "%s" v.name
  | "", Some i ->
    let new_name = ((!global_mutable_names_for_print).get_name_for_print v) in
    if String.equal new_name ""
    then Format.fprintf ppf "#%d" i
    else Format.fprintf ppf "'%s%s" new_name (int_to_unicode i)
  | _, None -> Format.fprintf ppf "%s" v.name
  | _, Some i -> Format.fprintf ppf "%s#%d" v.name i

module Int = Base.Int
module Option = Base.Option

let equal v1 v2 =
  String.equal v1.name v2.name
  && Option.equal Int.equal v1.counter v2.counter

let compare v1 v2 =
  let cname = String.compare v1.name v2.name in
  if Int.equal cname 0
  then Option.compare Int.compare v1.counter v2.counter
  else cname

let global_counter = ref 0

let reset_counter () = global_counter := 0

let of_name name =
  { name = name ;
    counter = None
  }

(* This exception indicates that some code tried to throw away the
   counter of a generated variable. It is not supposed to happen. *)
exception Tried_to_unfreshen_variable

(* TODO delete this *)
let to_name var =
  match var.counter with
  | None -> var.name
  | Some _ -> raise Tried_to_unfreshen_variable

let fresh ?name () =
  let name = Option.value ~default:"" name in
  let counter = incr global_counter ; Some !global_counter in
  { name ; counter }

let fresh_like v =
  fresh ~name:v.name ()

let debug v = match v.counter with Some c -> Printf.sprintf "%s(%d)" v.name c | None -> Printf.sprintf "%s(None)" v.name

let is_generated var =
  match var.counter with
  | None -> false
  | Some _ -> true

let internal_get_name_and_counter var = (var.name, var.counter)

let todo_cast : 'a 'b . 'a t -> 'b t = fun { name ; counter } -> { name ; counter }

let wildcard = "_"
