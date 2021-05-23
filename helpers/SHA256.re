open Mirage_crypto.Hash.SHA256;

let (let.ok) = Result.bind;

type hash = string;
let to_hex = str => {
  let `Hex(str) = Hex.of_string(str);
  str;
};
let of_hex = hex => {
  let size_in_bits = 256;
  let size_in_bytes = size_in_bits / 8;
  let size_in_hex = size_in_bytes * 2;
  if (String.length(hex) == size_in_hex) {
    // TODO: this can definitely raise an exception
    Ok(
      Hex.to_string(`Hex(hex)),
    );
  } else {
    Error("Invalid hash");
  };
};
let hash_to_yojson = str => `String(to_hex(str));
let hash_of_yojson = json => {
  let.ok hex = [%of_yojson: string](json);
  of_hex(hex);
};
let hash_to_string = to_hex;
let compare_hash = String.compare;
[@deriving yojson]
type t('a) = {
  hash,
  data: 'a,
};

let hash = data => {
  let hash = {
    let data = Marshal.to_string(data, []);
    Cstruct.of_string(data) |> feed(empty) |> get |> Cstruct.to_string;
  };
  {hash, data};
};
let verify = (~hash as expected_hash, data) => {
  let t = hash(data);
  t.hash == expected_hash ? Ok(t) : Error("Invalid hash");
};

let of_yojson = (f, json) =>
  Result.bind(of_yojson(f, json), ({hash, data}) => verify(~hash, data));
