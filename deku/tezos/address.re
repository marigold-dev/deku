open Crypto;
open Helpers;

[@deriving (eq, ord)]
type t =
  | Implicit(Key_hash.t)
  | Originated({
      contract: Contract_hash.t,
      entrypoint: option(string),
    });

let to_string =
  fun
  | Implicit(key_hash) => Key_hash.to_string(key_hash)
  | Originated({contract, entrypoint: None}) =>
    Contract_hash.to_string(contract)
  | Originated({contract, entrypoint: Some(entrypoint)}) =>
    Contract_hash.to_string(contract) ++ "%" ++ entrypoint;
let of_string = {
  let implicit = string => {
    let.some implicit = Key_hash.of_string(string);
    Some(Implicit(implicit));
  };
  let originated = string => {
    let.some (contract, entrypoint) =
      switch (String.split_on_char('%', string)) {
      | [contract] => Some((contract, None))
      | [contract, entrypoint]
          when String.length(entrypoint) < 32 && entrypoint != "default" =>
        Some((contract, Some(entrypoint)))
      | _ => None
      };
    let.some contract = Contract_hash.of_string(contract);
    Some(Originated({contract, entrypoint}));
  };
  Encoding_helpers.parse_string_variant([implicit, originated]);
};

let encoding = {
  open Data_encoding;

  // https://gitlab.com/tezos/tezos/-/blob/4627b7752c290717b603975aac5e56afdb7997a2/src/proto_alpha/lib_protocol/contract_repr.ml#L84
  let contract_encoding = {
    def(
      "contract_id",
      ~title="A contract handle",
      ~description=
        "A contract notation as given to an RPC or inside scripts. Can be a base58 implicit contract hash or a base58 originated contract hash.",
    ) @@
    union(
      ~tag_size=`Uint8,
      [
        case(
          Tag(0),
          ~title="Implicit",
          Key_hash.encoding,
          fun
          | Implicit(k) => Some(k)
          | _ => None,
          k =>
          Implicit(k)
        ),
        case(
          Tag(1),
          Fixed.add_padding(Contract_hash.encoding, 1),
          ~title="Originated",
          fun
          | Originated({contract, _}) => Some(contract)
          | _ => None,
          contract =>
          Originated({contract, entrypoint: None})
        ),
      ],
    );
  };

  let name = "address";
  let title = "An contract address optionally followed by an entrypoint.";

  // https://gitlab.com/tezos/tezos/-/blob/4627b7752c290717b603975aac5e56afdb7997a2/src/proto_alpha/lib_protocol/script_ir_translator.ml#L466
  let raw_encoding =
    conv(
      t =>
        switch (t) {
        | Implicit(_) as t => (t, "")
        | Originated({contract: _, entrypoint}) as t =>
          let entrypoint = Option.value(~default="", entrypoint);
          (t, entrypoint);
        },
      fun
      // TODO: should we just discard entrypoint of implicit?
      | (Implicit(_) as t, _) => t
      | (Originated({contract, _}), "" | "default") =>
        Originated({contract, entrypoint: None})
      | (Originated({contract, _}), entrypoint) =>
        Originated({contract, entrypoint: Some(entrypoint)}),
      tup2(contract_encoding, Variable.string),
    );
  Encoding_helpers.make_encoding(
    ~name,
    ~title,
    ~to_string,
    ~of_string,
    ~raw_encoding,
  );
};

let (to_yojson, of_yojson) =
  Yojson_ext.with_yojson_string("address", to_string, of_string);
