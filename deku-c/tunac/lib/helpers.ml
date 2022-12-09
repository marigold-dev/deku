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
end

module Map = struct
  include Map

  module type S = sig
    include Map.S
  end

  module Make (K : sig
    type t [@@deriving ord]
  end) =
  struct
    include Map.Make (K)
  end
end

module Set = struct
  include Set

  module type S = sig
    include Set.S
  end

  module Make (V : sig
    type t [@@deriving ord]
  end) =
  struct
    include Set.Make (V)
  end
end
