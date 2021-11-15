let global_mutable_pending_prints : (unit -> unit) list ref = ref []

let queue_print (print_thunk : unit -> unit) = global_mutable_pending_prints := print_thunk :: !global_mutable_pending_prints

let rec number_to_letters_ : int -> string = fun n ->
  let chr = match n mod 26 with
      0 -> "a" | 1 -> "b" | 2 -> "c" | 3 -> "d" | 4 -> "e" | 5 -> "f" | 6 -> "g" | 7 -> "h" | 8 -> "i" | 9 -> "j" | 10 -> "k" | 11 -> "l" | 12 -> "m" | 13 -> "n" | 14 -> "o" | 15 -> "p" | 16 -> "q" | 17 -> "r" | 18 -> "s" | 19 -> "t" | 20 -> "u" | 21 -> "v" | 22 -> "w" | 23 -> "x" | 24 -> "y" | 25 -> "z"
    | _ -> failwith (Format.asprintf "Cannot convert number %d to sequence of letters, is it a negative numer?" n)
  in
  if n = 0 then "" else number_to_letters_ (n / 26) ^ chr

let number_to_letters : int -> string = fun n ->
  if n = 0 then "a" else number_to_letters_ n

let flush_pending_print (state : _ Solver_types.typer_state) =
  let aux i vars =
    let letters = number_to_letters i in
    List.map ~f:(fun var -> (Var.internal_get_name_and_counter var, letters)) vars in
  let cmp = Pair.compare (String.compare) (Option.compare Int.compare) in
  let partition = UnionFind.Poly2.partitions state.aliases in
  if false then Format.(eprintf "Partition : %a\n%!" (Ast_core.PP.list_sep_d (Ast_core.PP.list_sep_d Var.pp)) partition);
  let vars_to_letters =
    try 
    Option.value_exn 
      (PolyMap.from_list ~cmp
      @@ List.concat
      @@ List.mapi ~f:aux
      @@ partition)
    with _ -> failwith "internal error: duplicate variables in union-find"
   in 
  let get_name_for_print = fun v ->
    fst @@ PolyMap.find_default (Var.internal_get_name_and_counter v) (fun () -> "") vars_to_letters in
  let () = Var.with_names_for_print
      Var.{ get_name_for_print }
      (fun () ->
         let apply thunk = thunk () in
         List.iter ~f: apply (List.rev !global_mutable_pending_prints)) in
  global_mutable_pending_prints := []
