module BLAKE2B = BLAKE2B;
module BLAKE2B_20 = BLAKE2B.BLAKE2B_20;
module Incremental_patricia = Incremental_patricia;

module List = List_ext;
module Z = Zarith_ext;
module Uri = Uri_ext;
module Result = Result_ext;
module Option = Option_ext;
module Yojson_ext = Yojson_ext;
module Map = Map_ext;
module Set = Set_ext;
module String_map =
  Map.Make_with_yojson({
    [@deriving yojson]
    type t = string;
    let compare = String.compare;
  });

module String_set =
  Set.Make_with_yojson({
    [@deriving yojson]
    type t = string;
    let compare = String.compare;
  });

module Int64_map =
  Map.Make_with_yojson({
    [@deriving yojson]
    type t = int64;
    let compare = Int64.compare;
  });

include Lwt_ext.Let_syntax;
include Result.Let_syntax;
include Option.Let_syntax;
