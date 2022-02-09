open Protocol
type t
val make : self_key:Wallet.t -> t
val is_self_signed : t -> bool
val is_signed : t -> bool
val set_signed : t -> t
val add : signatures_required:int -> Signature.t -> t -> t
val mem : Signature.t -> t -> bool
val to_list : t -> Signature.t List.t
