open List

type usage =
| Drop
| Keep

(** val filter_keeps : usage list -> usage list **)

let filter_keeps us =
  filter (fun u -> match u with
                   | Drop -> false
                   | Keep -> true) us

(** val select : usage list -> 'a1 list -> 'a1 list **)

let rec select us xs =
  match us with
  | [] -> xs
  | u :: us0 ->
    (match u with
     | Drop -> (match xs with
                | [] -> []
                | _ :: xs0 -> select us0 xs0)
     | Keep -> (match xs with
                | [] -> []
                | x :: xs0 -> x :: (select us0 xs0)))

type side =
| Left
| Right
| Both

type splitting = side list

(** val keep_right : usage -> side **)

let keep_right = function
| Drop -> Right
| Keep -> Both

(** val keep_rights : usage list -> side list **)

let keep_rights us =
  map keep_right us

(** val left_usage : side -> usage **)

let left_usage = function
| Right -> Drop
| _ -> Keep

(** val right_usage : side -> usage **)

let right_usage = function
| Left -> Drop
| _ -> Keep

(** val left_usages : splitting -> usage list **)

let left_usages ss =
  map left_usage ss

(** val right_usages : splitting -> usage list **)

let right_usages ss =
  map right_usage ss

(** val split : splitting -> 'a1 list -> 'a1 list * 'a1 list **)

let split ss xs =
  ((select (left_usages ss) xs), (select (right_usages ss) xs))

(** val assoc_splitting1 : splitting -> splitting -> splitting **)

let rec assoc_splitting1 outer inner =
  match outer with
  | [] -> []
  | s :: outer0 ->
    (match s with
     | Left ->
       (match inner with
        | [] -> []
        | s0 :: inner0 -> s0 :: (assoc_splitting1 outer0 inner0))
     | Right -> Right :: (assoc_splitting1 outer0 inner)
     | Both ->
       (match inner with
        | [] -> []
        | s0 :: inner0 ->
          (match s0 with
           | Left -> Both :: (assoc_splitting1 outer0 inner0)
           | x -> x :: (assoc_splitting1 outer0 inner0))))

(** val assoc_splitting2 : splitting -> splitting -> splitting **)

let rec assoc_splitting2 outer inner =
  match outer with
  | [] -> []
  | s :: outer0 ->
    (match s with
     | Left ->
       (match inner with
        | [] -> []
        | s0 :: inner0 ->
          (match s0 with
           | Left -> assoc_splitting2 outer0 inner0
           | _ -> Left :: (assoc_splitting2 outer0 inner0)))
     | Right -> Right :: (assoc_splitting2 outer0 inner)
     | Both ->
       (match inner with
        | [] -> []
        | s0 :: inner0 ->
          (match s0 with
           | Left -> Right :: (assoc_splitting2 outer0 inner0)
           | _ -> Both :: (assoc_splitting2 outer0 inner0))))

(** val assoc_splitting : splitting -> splitting -> splitting * splitting **)

let assoc_splitting outer inner =
  ((assoc_splitting1 outer inner), (assoc_splitting2 outer inner))

(** val flip_side : side -> side **)

let flip_side = function
| Left -> Right
| Right -> Left
| Both -> Both

(** val flip_splitting : splitting -> splitting **)

let flip_splitting ss =
  map flip_side ss

(** val union : usage list -> usage list -> splitting * usage list **)

let rec union ls rs =
  match ls with
  | [] -> ([], [])
  | l :: ls0 ->
    (match rs with
     | [] -> ([], [])
     | r :: rs0 ->
       let (ss, us) = union ls0 rs0 in
       (match l with
        | Drop ->
          (match r with
           | Drop -> (ss, (Drop :: us))
           | Keep -> ((Right :: ss), (Keep :: us)))
        | Keep ->
          (match r with
           | Drop -> ((Left :: ss), (Keep :: us))
           | Keep -> ((Both :: ss), (Keep :: us)))))
