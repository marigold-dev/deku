module Make (P : sig type meta end) = struct
  type meta = P.meta
  type 'value t = {
    value : 'value ;
    meta : meta ;
  }

  let make meta value = { value ; meta }
  let value t = t.value
  let meta t = t.meta

  let apply : ('a -> 'b) -> 'a t -> 'b = fun f x -> f x.value
end

module Location = struct
  include Make(struct type meta = Location.t end)

  let make_f f : loc:_ -> _ -> _ t = fun ~loc x ->  make loc (f x)
  let make ~loc x : _ t = make loc x
  let update_location ~loc t = {t with meta = loc}
end
