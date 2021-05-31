include Digestif.Make_BLAKE2B({
  let digest_size = 32;
});
let (let.ok) = Result.bind;

let to_yojson = str => `String(to_hex(str));
let of_yojson = json => {
  let.ok hex = [%of_yojson: string](json);
  try(Ok(of_hex(hex))) {
  | Invalid_argument(invalid) => Error(invalid)
  };
};
let to_raw_string = to_raw_string;
let to_string = to_hex;
// TODO: check the unsafe here
let compare = unsafe_compare;

let hash = data => digest_string(data);
let verify = (~hash as expected_hash, data) => expected_hash == hash(data);

// TODO: magic means evil
module Magic = {
  [@deriving yojson]
  type hash = t;
  [@deriving yojson]
  type t('a) = {
    hash,
    data: 'a,
  };
  let hash = data => {
    let hash = hash(Marshal.to_string(data, []));
    {hash, data};
  };
  let verify = (~hash as expected_hash, data) => {
    let t = hash(data);
    t.hash == expected_hash ? Ok(t) : Error("Invalid hash");
  };
  let of_yojson = (f, json) =>
    Result.bind(of_yojson(f, json), ({hash, data}) => verify(~hash, data));
};
