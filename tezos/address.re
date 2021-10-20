open Helpers;
[@deriving eq]
type t =
  | Implicit(Key_hash.t)
  | Originated({
      contract: Contract_hash.t,
      entrypoint: option(string),
    });

let contract_encoding =
  Data_encoding.(
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
    )
  );

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
  List.try_decode_list([implicit, originated]);
};
let encoding =
  Data_encoding.(
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
    )
  );

let with_yojson_string = (name, of_string, to_string) =>
  Yojson_ext.with_yojson_string(
    string =>
      of_string(string) |> Option.to_result(~none="invalid " ++ name),
    to_string,
  );
let (of_yojson, to_yojson) =
  with_yojson_string("address", of_string, to_string);
