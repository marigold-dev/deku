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
    | PushRetAddr of t list
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
    | PushRetAddr of t list
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

  open Zt

  let rec of_typed lst = List.map ~f:typed_match lst

  and typed_match = function
    | Zinc.Core c -> of_core c
    | Zinc.Plain_old_data c -> of_plain c
    | Zinc.Adt c -> of_adt c
    | Zinc.Operation o -> of_operation o
    | Zinc.Domain_specific_operation d -> of_domain_specific_operation d
    | Zinc.Control_flow c -> of_control_flow c

  and of_core = function
    | Zinc.Grab -> Grab
    | Zinc.Return -> Return
    | Zinc.Access n -> Access n
    | Zinc.Apply -> Apply
    | Zinc.Closure z -> Closure (of_typed z)
    | Zinc.EndLet -> EndLet
    | Zinc.PushRetAddr z -> PushRetAddr (of_typed z)

  and of_plain = function
    | Zinc.Bool b -> Bool b
    | Zinc.String s -> String s
    | Zinc.Num z -> Num z
    | Zinc.Mutez z -> Mutez z
    | Zinc.Nil -> Nil
    | Zinc.Bytes b -> Bytes b
    | Zinc.Address a -> Address a
    | Zinc.Key a -> Key a
    | Zinc.Hash a -> Hash a
    | Zinc.Key_hash a -> Key_hash a
    | Zinc.Chain_id c -> Chain_id c

  and of_adt = function
    | Zinc.MakeRecord n -> MakeRecord n
    | Zinc.MakeVariant n -> MakeVariant n
    | Zinc.MatchVariant t -> MatchVariant (Array.map ~f:of_typed t)
    | Zinc.RecordAccess n -> RecordAccess n

  and of_operation = function
    | Zinc.Eq -> Eq
    | Zinc.Add -> Add
    | Zinc.Cons -> Cons
    | Zinc.HashKey -> HashKey
    | Zinc.Or -> Or
    | Zinc.And -> And
    | Zinc.Not -> Not

  and of_domain_specific_operation = function
    | Zinc.ChainID -> ChainID
    | Zinc.Contract_opt -> Contract_opt
    | Zinc.MakeTransaction -> MakeTransaction

  and of_control_flow = function Zinc.Failwith -> Failwith

  let rec of_typed_stack lst = List.map ~f:of_stack_match lst

  and of_stack_match = function
    | Stack_item.Z d -> typed_match d
    | Stack_item.NonliteralValue d -> of_nonlit d
    | Stack_item.Clos {Clos.code; env} ->
        Clos {code = of_typed code; env = of_env env}
    | Stack_item.Record r -> Record (Array.map ~f:of_stack_match r)
    | Stack_item.List l -> List (of_typed_stack l)
    | Stack_item.Variant (l, t) -> Variant {tag = l; value = of_stack_match t}
    | Stack_item.Marker (instructions, env) ->
        Marker {code = of_typed instructions; env = of_env env}

  and of_env lst = List.map ~f:of_env_match lst

  and of_env_match = function
    | Env_item.Z d -> typed_match d
    | Env_item.NonliteralValue d -> of_nonlit d
    | Env_item.Clos {Clos.code; env} ->
        Clos {code = of_typed code; env = of_env env}
    | Env_item.Record r -> Record (Array.map ~f:of_stack_match r)
    | Env_item.List l -> List (of_typed_stack l)
    | Env_item.Variant (l, t) -> Variant {tag = l; value = of_stack_match t}

  and of_nonlit = function
    | Zinc.Contract c -> Contract c
    | Zinc.Chain_operation (Zinc.Transaction (x, y)) -> Transaction (x, y)
end
