type set = {
  key : string;
  value : Yojson.Safe.t;
}
[@@deriving yojson]

type vm_message =
  | Init  of set list
  | Stop
  | Set   of set
  | Get   of string
  | Error of string
[@@deriving yojson]

module State : sig
  type t [@@deriving yojson]

  val get : string -> t -> Yojson.Safe.t option

  val set : string -> Yojson.Safe.t -> t -> t

  val empty : t
end
