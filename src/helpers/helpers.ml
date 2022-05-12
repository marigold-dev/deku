module List = List_ext
module Z = Zarith_ext
module Uri = Uri_ext
module Result = Result_ext
module Option = Option_ext
module Yojson_ext = Yojson_ext
module Map = Map_ext
module Set = Set_ext
module String_map = Map.Make_with_yojson (struct
  type t = string [@@deriving yojson]
  let compare = String.compare
end)
module String_set = Set.Make_with_yojson (struct
  type t = string [@@deriving yojson]
  let compare = String.compare
end)
module Int64_map = Map.Make_with_yojson (struct
  type t = int64 [@@deriving yojson]
  let compare = Int64.compare
end)
module Parallel = Parallel
include Lwt_ext.Let_syntax
include Result.Let_syntax
include Option.Let_syntax
