module type S = sig
  type key

  type 'a t [@@deriving yojson, eq]

  val empty : 'a t

  (* O(log n) *)
  val add : key -> 'a -> 'a t -> 'a t

  (* O(1) *)
  val cardinal : 'a t -> int

  (* O(log n) *)
  val find : key -> 'a t -> 'a option
end

(* Map.S + O(1) cardinal*)
module Make (K : sig
  type t [@@deriving yojson]
  val compare : t -> t -> int
end) : S with type key = K.t = struct
  type key = K.t

  module Map = struct
    include Map.Make (K)

    let to_yojson f t = t |> bindings |> [%to_yojson: (K.t * 'a) list] f
    let of_yojson f json =
      json
      |> [%of_yojson: (K.t * 'a) list] f
      |> Result.map (fun l -> l |> List.to_seq |> of_seq)
  end
  type 'a t = {
    cardinality : int;
    values : 'a Map.t;
  }
  [@@deriving yojson, eq]

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
end
