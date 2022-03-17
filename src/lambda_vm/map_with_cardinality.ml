module type S = sig
  type key

  type 'a t

  val empty : 'a t

  (* O(log n) *)
  val add : key -> 'a -> 'a t -> 'a t

  (* O(1) *)
  val cardinal : 'a t -> int

  (* O(log n) *)
  val find : key -> 'a t -> 'a option
  
  val equal : ('a -> 'a -> bool) -> 'a t -> 'a t -> bool
end

(* Map.S + O(1) cardinal*)
module Make (K : Map.OrderedType) : S with type key = K.t = struct
  type key = K.t

  module Map = Map.Make (K)
  type 'a t = {
    cardinality : int;
    values : 'a Map.t;
  }

  let empty = { cardinality = 0; values = Map.empty }

  let add key value t =
    let { cardinality; values } = t in
    let cardinality =
      (* TODO: this could be fused with Ident.Map.add by using Ident.Map.update *)
      if Map.mem key values then
        cardinality
      else
        cardinality + 1 in
    let values = Map.add key value values in
    { cardinality; values }

  let cardinal t = t.cardinality
  let find key t = Map.find_opt key t.values

  let equal f { cardinality = c1; values = m1 }
      { cardinality = c2; values = m2 } =
    Int.equal c1 c2 && Map.equal f m1 m2
end
