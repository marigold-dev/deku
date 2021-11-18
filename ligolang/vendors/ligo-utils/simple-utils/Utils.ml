(* Utility types and functions *)

(* Identity *)

let id x = x

(* Combinators *)

let (<@) f g x = f (g x)

let swap f x y = f y x

let lambda = fun x _ -> x

let curry f x y = f (x,y)
let uncurry f (x,y) = f x y

(* Parametric rules for sequences *)

type        'a    nseq = 'a * 'a list
type ('a,'sep) nsepseq = 'a * ('sep * 'a) list
type ('a,'sep)  sepseq = ('a,'sep) nsepseq option

(* Consing *)

let nseq_cons x (hd,tl) = x, hd::tl
let nsepseq_cons x sep (hd,tl) = x, (sep,hd)::tl

let sepseq_cons x sep = function
          None -> x, []
| Some (hd,tl) -> x, (sep,hd)::tl

(* Rightwards iterators *)

let nseq_foldl f a (hd,tl) = List.fold_left f a (hd::tl)

let nsepseq_foldl f a (hd,tl) =
  List.fold_left (fun a (_,e) -> f a e) (f a hd) tl

let sepseq_foldl f a = function
    None -> a
| Some s -> nsepseq_foldl f a s

let nseq_iter f (hd,tl) = List.iter f (hd::tl)

let nsepseq_iter f (hd,tl) = f hd; List.iter (f <@ snd) tl

let sepseq_iter f = function
    None -> ()
| Some s -> nsepseq_iter f s

(* Reversing *)

let nseq_rev (hd,tl) =
  let rec aux acc = function
      [] -> acc
  | x::l -> aux (nseq_cons x acc) l
in aux (hd,[]) tl

let nsepseq_rev =
  let rec aux acc = function
    hd, (sep,snd)::tl -> aux ((sep,hd)::acc) (snd,tl)
  | hd,            [] -> hd, acc in
function
  hd, (sep,snd)::tl -> aux [sep,hd] (snd,tl)
|                 s -> s

let sepseq_rev = function
      None -> None
| Some seq -> Some (nsepseq_rev seq)

(* Leftwards iterators *)

let nseq_foldr f (hd,tl) = List.fold_right f (hd::tl)

let nsepseq_foldr f (hd,tl) a = f hd (List.fold_right (f <@ snd) tl a)

let sepseq_foldr f = function
    None -> fun a -> a
| Some s -> nsepseq_foldr f s

(* Maps *)

let nseq_map f (hd,tl) = f hd, List.map f tl

let nsepseq_map f (hd,tl) =
  f hd, List.map (fun (sep,item) -> (sep, f item)) tl

let sepseq_map f = function
      None -> None
| Some seq -> Some (nsepseq_map f seq)

(* Conversions to lists *)

let nseq_to_list (x,y) = x::y

let nsepseq_to_list (x,y) = x :: List.map snd y

let sepseq_to_list = function
    None -> []
| Some s -> nsepseq_to_list s

(* Optional values *)

module Option =
  struct
    let apply f x =
      match x with
        Some y -> Some (f y)
      |   None -> None

    let rev_apply x y =
      match x with
        Some f -> f y
      |   None -> y

    let to_string = function
      Some x -> x
    |   None -> ""
  end

(* Modules based on [String], like sets and maps. *)

module String =
  struct
    include String

    module Ord =
      struct
        type nonrec t = t
        let compare = compare
      end

    module Map = Map.Make (Ord)
    module Set = Set.Make (Ord)
  end

(* Integers *)

module Int =
  struct
    type t = int

    module Ord =
      struct
        type nonrec t = t
        let compare = compare
      end

    module Map = Map.Make (Ord)
    module Set = Set.Make (Ord)
  end

(* Effectful symbol generator *)

let gen_sym =
  let counter = ref 0 in
  fun () -> incr counter; "v" ^ string_of_int !counter

(* General tracing function *)

let trace text = function
       None -> ()
| Some chan -> output_string chan text; flush chan

(* Printing a string in red to standard error *)

let highlight msg = Printf.eprintf "\027[31m%s\027[0m%!" msg

(* When failing to parse a specifed JSON format *)
let error_yojson_format format =
  Error ("Invalid JSON value.
          An object with the following specification is expected:"
         ^ format)
