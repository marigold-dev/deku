open Format

let string : formatter -> string -> unit = fun ppf s -> fprintf ppf "%s" s

let tag tag : formatter -> unit -> unit = fun ppf () -> fprintf ppf tag

let bool ppf b = fprintf ppf "%b" b

let pair f g ppf (a , b) = fprintf ppf "(%a , %a)" f a g b

let new_line : formatter -> unit -> unit = tag "@;"

let rec new_lines n ppf () =
  match n with
  | 0 -> new_line ppf ()
  | n -> new_line ppf () ; new_lines (n-1) ppf ()

let const const : formatter -> unit -> unit = fun ppf () -> fprintf ppf "%s" const

let comment : formatter -> string -> unit = fun ppf s -> fprintf ppf "(* %s *)" s

let list_sep value separator = pp_print_list ~pp_sep:separator value
let list_sep_d x = list_sep x (tag " ,@ ")
let list_sep_d_par f ppf lst =
  match lst with 
  | [] -> ()
  | _ -> fprintf ppf " (%a)" (list_sep_d f) lst

let list value = pp_print_list ~pp_sep:(tag "") value

let ne_list_sep value separator ppf (hd, tl) =
  value ppf hd ;
  separator ppf () ;
  pp_print_list ~pp_sep:separator value ppf tl

let prepend s f ppf a =
  fprintf ppf "%s%a" s f a

let option = fun f ppf opt ->
  match opt with
  | Some x -> fprintf ppf "Some(%a)" f x
  | None -> fprintf ppf "None"

let lr = fun ppf lr ->
  match lr with
  | `Left -> fprintf ppf "left"
  | `Right -> fprintf ppf "right"

let int = fun ppf n -> fprintf ppf "%d" n

let map = fun f pp ppf x ->
  pp ppf (f x)

let pair_sep value sep ppf (a, b) =
  fprintf ppf "%a %s %a" value a sep value b

let smap_sep value sep ppf m =
  let module SMap = X_map.String in
  let lst = SMap.to_kv_list m in
  let new_pp ppf (k, v) = fprintf ppf "%s -> %a" k value v in
  fprintf ppf "%a" (list_sep new_pp sep) lst
