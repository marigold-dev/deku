module type S = sig
  open Base
  open Zinc_utils

  module Key : sig
    type t
  end

  module Address : sig
    type t
  end

  module Contract : sig
    type t
  end

  module Chain_id : sig
    type t
  end

  module Hash : sig
    type t
  end

  module Key_hash : sig
    type t
  end

  module Zt :
    Zinc_types.S
      with type Zinc.Key.t := Key.t
       and type Zinc.Address.t := Address.t
       and type Zinc.Contract.t := Contract.t
       and type Zinc.Chain_id.t := Chain_id.t
       and type Zinc.Hash.t := Hash.t
       and type Zinc.Key_hash.t := Key_hash.t

  module Z : sig
    include module type of struct
      include Zinc_utils.Z
    end

    include Bin_prot.Binable.Minimal.S with type t := t
  end

  type t =
    | Grab
    | Return
    | PushRetAddr of t
    | Apply
    | Access of int
    | Closure of t list
    | EndLet
    | Bool of bool
    | String of string
    | Num of Z.t
    | Mutez of Z.t
    | Nil
    | Bytes of bytes
    | Address of Address.t
    | Key of Key.t
    | Hash of Hash.t
    | Key_hash of Key_hash.t
    | Chain_id of Chain_id.t
    | MakeRecord of int
    | RecordAccess of int
    | MakeVariant of int
    | MatchVariant of t list LMap.t
    | Eq
    | Add
    | Cons
    | HashKey
    | Or
    | And
    | Not
    | ChainID
    | Contract_opt
    | MakeTransaction
    | Failwith
    (* nonliteral values *)
    | Contract of Contract.t
    (* chain operations *)
    | Transaction of Z.t * Contract.t
    | Clos of {code : t list; env : t list}
    | Record of t LMap.t
    | List of t list
    | Variant of {tag : int; value : t}
    | Marker of {code : t list; env : t list}
  [@@deriving bin_io]
end

module Make (D : Zinc_types.Domain_types) :
  S
    with type Key.t := D.Key.t
     and type Address.t := D.Address.t
     and type Contract.t := D.Contract.t
     and type Chain_id.t := D.Chain_id.t
     and type Hash.t := D.Hash.t
     and type Key_hash.t := D.Key_hash.t
