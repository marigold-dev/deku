module Result = struct
  include Result

  module Let_syntax = struct
    let ( let* ) a f = Result.bind a f

    let ( let+ ) a f = Result.map f a
  end

  module Infix = struct
    let ( >>= ) a f = Result.bind a f

    let ( >>| ) a f = Result.map f a
  end

  let wrap error ~f = Result.map_error f error
end

module Option = struct
  include Option

  module Let_syntax = struct
    let ( let* ) a f = Option.bind a f

    let ( let+ ) a f = Option.map f a
  end

  module Infix = struct
    let ( >>= ) a f = Option.bind a f

    let ( >>| ) a f = Option.map f a
  end
end

module Z = struct
  include Z

  let yojson_of_t t = `String (Z.to_string t)

  let t_of_yojson = function
    | `String string -> Z.of_string string
    | _ -> failwith "invalid type"
end

module Map = struct
  include Map

  module type S_with_yojson = sig
    include Map.S

    val yojson_of_t : ('a -> Yojson.Safe.t) -> 'a t -> Yojson.Safe.t

    val t_of_yojson : (Yojson.Safe.t -> 'a) -> Yojson.Safe.t -> 'a t
  end

  module Make_with_yojson (K : sig
    type t [@@deriving ord, yojson]
  end) =
  struct
    include Map.Make (K)

    let yojson_of_t f t : Yojson.Safe.t =
      let bindings = bindings t in
      `List (List.map (fun (k, v) -> `List [ K.yojson_of_t k; f v ]) bindings)

    let t_of_yojson f (json : Yojson.Safe.t) =
      match json with
      | `List l ->
        List.map
          (function
            | `List [ k; v ] -> (K.t_of_yojson k, f v)
            | _ -> failwith "invalid arg")
          l
        |> List.to_seq |> of_seq
      | _ -> failwith "invalid arg"
  end
end

module Set = struct
  include Set

  module type S_with_yojson = sig
    include Set.S

    val yojson_of_t : t -> Yojson.Safe.t

    val t_of_yojson : Yojson.Safe.t -> t
  end

  module Make_with_yojson (V : sig
    type t [@@deriving ord, yojson]
  end) =
  struct
    include Set.Make (V)

    let yojson_of_t t =
      `List (fold (fun x acc -> V.yojson_of_t x :: acc) t [] |> List.rev)

    let t_of_yojson json =
      match json with
      | `List l -> of_list (List.map V.t_of_yojson l)
      | _ -> failwith "invalid arg"
  end
end
