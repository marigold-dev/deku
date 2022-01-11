open Zinc_utils
open Base
open Bin_prot.Std

module type S = sig
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
      include Z
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
    | MatchVariant of t list Zinc_utils.LMap.t
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
    | Record of t Zinc_utils.LMap.t
    | List of t list
    | Variant of {tag : int; value : t}
    | Marker of {code : t list; env : t list}
  [@@deriving bin_io]

  val of_typed : Zt.Zinc.t -> t

  val of_typed_stack : Zt.Stack.t -> t
end

module Make (D : Zinc_types.Domain_types) = struct
  module Zt = Zinc_types.Make (D)

  module Z : sig
    include module type of struct
      include Zinc_utils.Z
    end

    include Bin_prot.Binable.Minimal.S with type t := t
  end = struct
    include Z

    include struct
      let __bin_read_t__ buf ~pos_ref t =
        Z.of_bits @@ __bin_read_string__ buf ~pos_ref t

      let bin_read_t buf ~pos_ref = Z.of_bits @@ bin_read_string buf ~pos_ref

      let bin_write_t buf ~pos t = bin_write_string buf ~pos (Z.to_bits t)

      let bin_size_t str = bin_size_string (Z.to_bits str)

      let bin_shape_t = bin_shape_string
    end
  end

  module Key = D.Key
  module Key_hash = D.Key_hash
  module Hash = D.Hash
  module Chain_id = D.Chain_id
  module Address = D.Address
  module Contract = D.Contract

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

  let of_typed = failwith "Simple"

  let of_typed_stack = failwith "simple"
end
