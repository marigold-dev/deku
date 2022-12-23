type t =
  | View of {
      address : Deku_ledger.Contract_address.t;
      view_name : string;
      argument : Value.t;
    }
  | Call of { address : Deku_ledger.Address.t; argument : Value.t }
  | Originate of {
      initial_storage : Value.t;
      module_ : string;
      entrypoints : Entrypoints.t;
      constants : (int * Value.t) array;
    }
[@@deriving show, yojson]

exception Not_an_operation

val encoding : t Data_encoding.t
