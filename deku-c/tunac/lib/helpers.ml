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

module Z = Z

module Map = Map

module Set = Set
