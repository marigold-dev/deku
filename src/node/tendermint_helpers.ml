module CI = Tendermint_internals
module type COUNTER = sig
  type 'a t

  val make : 'a t

  val inc : 'a t -> 'a -> int -> 'a t

  val get : 'a t -> 'a -> int

  val count : ('a * int) list -> 'a t

  val cut : 'a t -> threshold:float -> 'a t

  val filter_threshold : ('a * int) list -> threshold:float -> 'a list
end

module Counter : COUNTER = struct
  (* Why is this not in stdlib??? *)
  type 'a t = ('a * int) list

  let make = []

  let inc t key n =
    let rec aux = function
      | (k, v) :: xs when k = key -> (k, n + v) :: xs
      | c :: xs -> c :: aux xs
      | [] -> [(key, n)] in
    aux t

  let get t key = List.assoc key t

  let count (ls : ('a * int) list) : 'a t =
    List.fold_left (fun counter (x, n) -> inc counter x n) make ls

  let cut t ~(threshold : float) =
    List.filter (fun (_a, b) -> float_of_int b >= threshold) t

  let filter_threshold (ls : ('a * int) list) ~(threshold : float) =
    let counted = count ls in
    let filtered = cut counted ~threshold in
    List.map fst filtered
end

module HeightHash = struct
  type t = CI.height
  let equal i j = i = j
  let hash i = Int64.(logand i max_int |> to_int)
end

(* FIXME: find another name *)
module IntSet = struct
  include Hashtbl.Make (HeightHash)

  let map_inplace f t = filter_map_inplace (fun x y -> Option.some (f x y)) t
end