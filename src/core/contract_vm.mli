module Contract : sig
  type t [@@deriving yojson, eq]
end
module Origination_payload : sig
  type t [@@deriving yojson]

  val lambda_of_yojson :
    code:Yojson.Safe.t -> storage:Yojson.Safe.t -> (t, string) result
end
module Compiler : sig
  val compile : Origination_payload.t -> gas:int -> (Contract.t, string) result
end
