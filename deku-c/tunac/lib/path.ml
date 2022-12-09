type path = Left | Right

module M = struct
  include Map.Make (String)
end

type t = path list M.t

let empty = M.empty
