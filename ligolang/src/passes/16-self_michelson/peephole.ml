(* A [peep] describes an optimizer of sequences, which can "peep" at
   elements one by one. *)
type 'a peep =
  (* Either return the sequence with no change, *)
  | No_change
  (* or return a new prefix, replacing all elements peeped so far, *)
  | Changed of 'a list
  (* or "peep" at the next element and then decide what to do (when
     there was no next element, Peep is equivalent to No_change.) *)
  | Peep of ('a -> 'a peep)
  (* or "peek" at the next element (if any) and then decide what to
     do, while leaving the element in place *)
  | Peek of ('a option -> 'a peep)

let peep_of_option : 'a list option -> 'a peep = function
  | None -> No_change
  | Some xs -> Changed xs

(* A let* syntax for Peep, see example usage in peepN below *)
type peep_dummy = Peep_dummy | Peek_dummy
let peep = Peep_dummy
let peek = Peek_dummy
module Let_syntax = struct
  let bind : peep_dummy -> f:('a -> 'a peep) -> 'a peep =
    fun _ ~f -> Peep f
  let (let*) x f = bind ~f x
end
open Let_syntax

(* These are used for backwards compatibility with the legacy
   definitions in self_michelson.ml. They can be less efficient than
   defining a peep directly (using the Let_syntax), because a peepN
   will always examine N elements, even if the first element is
   sufficient to return No_change. *)
type 'a peep1 = 'a -> 'a list option
type 'a peep2 = 'a * 'a -> 'a list option
type 'a peep3 = 'a * 'a * 'a -> 'a list option
type 'a peep4 = 'a * 'a * 'a * 'a -> 'a list option
type 'a peep5 = 'a * 'a * 'a * 'a * 'a -> 'a list option

let peep1 (f : _ peep1) : _ peep =
  let* x1 = peep in
  peep_of_option (f x1)

let peep2 (f : _ peep2) : _ peep =
  let* x1 = peep in
  let* x2 = peep in
  peep_of_option (f (x1, x2))

let peep3 (f : _ peep3) : _ peep =
  let* x1 = peep in
  let* x2 = peep in
  let* x3 = peep in
  peep_of_option (f (x1, x2, x3))

let peep4 (f : _ peep4) : _ peep =
  let* x1 = peep in
  let* x2 = peep in
  let* x3 = peep in
  let* x4 = peep in
  peep_of_option (f (x1, x2, x3, x4))

let peep5 (f : _ peep5) : _ peep =
  let* x1 = peep in
  let* x2 = peep in
  let* x3 = peep in
  let* x4 = peep in
  let* x5 = peep in
  peep_of_option (f (x1, x2, x3, x4, x5))

(* Unused "left to right" peep interpreter, left here for a moment so
   that it will be preserved in git history. Feel free to remove. *)
(*
let rec peephole (f : _ peep) (xs : _ list) : bool * _ list =
  (* apply the peep *)
  let rec dopeep f xs =
    match f with
    | No_change -> None
    | Changed popped' -> Some (popped' @ xs)
    | Peep f ->
      (match xs with
       | [] -> None
       | x :: xs' ->
         let f = f x in
         dopeep f xs') in
  (* iterate the peep at the head of the list *)
  let rec loop changed xs =
    match dopeep f xs with
    | None ->
      if_changed changed xs
    | Some xs' ->
      loop true xs' in
  let (changed, xs) =
    is_changed (loop false) xs in
  (* finally move on to the tail of the list *)
  match xs with
  | [] ->
    (changed, [])
  | x :: xs ->
    let (changed', xs') = peephole f xs in
    (changed || changed', x :: xs')
*)

(* "right to left" peep interpreter *)
let peephole (f : _ peep) (xs : _ list) : bool * _ list =
  (* applying the peep once *)
  let rec dopeep f xs =
    match f with
    | No_change -> None
    | Changed popped' -> Some (popped' @ xs)
    | Peep f ->
      (match xs with
       | [] -> None
       | x :: xs' ->
         let f = f x in
         dopeep f xs')
    | Peek f ->
      let f = f (match xs with [] -> None | x :: _ -> Some x) in
      dopeep f xs in
  (* try applying the peep to all tails of the sequence *)
  let rec aux changed xs xs' : bool * _ list =
    match dopeep f xs' with
    | None ->
      (match xs with
       (* done *)
       | [] -> (changed, xs')
       (* done with this tail, continue with next element *)
       | x :: xs -> aux changed xs (x :: xs'))
    | Some xs' ->
      aux true xs xs' in
  aux false (List.rev xs) []

(* `par2` and `par` are not used currently because they result in
   slightly worse code in example contracts, due to optimization
   ordering differences. Hmm... *)

(* Run two peeps in parallel *)
let rec par2 (f : 'a peep) (g : 'a peep) : 'a peep =
  match (f, g) with
  | (No_change, g) -> g
  | (Changed xs, _) -> Changed xs
  | (f, No_change) -> f
  | (_, Changed xs) -> Changed xs
  | (Peek f, g) ->
    Peek (fun x -> par2 (f x) g)
  | (f, Peek g) ->
    Peek (fun x -> par2 f (g x))
  | (Peep f, Peep g) ->
    Peep (fun x -> par2 (f x) (g x))

(* Run list of peeps in parallel *)
let par (fs : 'a peep list) : 'a peep =
  List.fold_left ~f:par2 ~init:No_change fs
