(* open Trace *)
open Types

(* module type ENVIRONMENT = sig
 *   type element = environment_element
 *   type t = environment
 *
 *   val empty : t
 *   val add : element -> t -> t
 *   val concat : t list -> t
 *   val get_opt : string -> t -> type_value option
 *   val get_i : string -> t -> (type_value * int)
 *   val of_list : element list -> t
 *   val closure_representation : t -> type_value
 * end *)

module Environment (* : ENVIRONMENT *) = struct
  type element = environment_element
  type t = environment

  let compare_var : expression_variable -> expression_variable -> int =
    fun a b -> Var.compare a.wrap_content b.wrap_content
  
  let var_equal = Location.equal_content ~equal:Var.equal
  let empty : t = []
  let add : element -> t -> t  = List.cons
  let concat : t list -> t  = List.concat
  let get_opt : expression_variable -> t -> type_expression option = fun e lst -> List.Assoc.find ~equal:var_equal lst e
  let has : expression_variable -> t -> bool = fun s t ->
    match get_opt s t with
    | None -> false
    | Some _ -> true
  let get_i : expression_variable -> t -> (type_expression * int) =fun x lst -> List.find_mapi_exn ~f:(fun i (e,t) -> if var_equal e x then Some (t,i) else None) lst
  let of_list : element list -> t = fun x -> x
  let to_list : t -> element list = fun x -> x
  let get_names : t -> expression_variable list = List.map ~f:fst
  let remove : int -> t -> t = List.remove

  let select ?(rev = false) ?(keep = true) : expression_variable list -> t -> t = fun lst env ->
    let e_lst =
      let e_lst = to_list env in
      let aux selector (s , _) =
        match List.mem ~equal:var_equal selector s with
        | true -> List.remove_element ~compare:compare_var s selector , keep
        | false -> selector , not keep in
      let e_lst' =
        if rev = keep
        then snd @@ List.fold_map ~f:aux ~init:lst e_lst
        else snd @@ List.fold_map ~f:aux ~init:lst @@ List.rev e_lst
      in
      let e_lst'' = List.zip_exn e_lst e_lst' in
      e_lst'' in
    of_list
    @@ List.map ~f:fst
    @@ List.filter ~f:snd
    @@ e_lst


  let fold : _ -> 'a -> t -> 'a = fun f init -> List.fold_left ~f ~init
  let filter : _ -> t -> t = fun f -> List.filter ~f
end

include Environment
