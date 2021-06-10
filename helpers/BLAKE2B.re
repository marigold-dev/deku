// TODO: this is bad, size shuold be a detail in a function
module Make =
       (P: {let size: int;})
       : {
         [@deriving yojson]
         type t;
         let to_string: t => string;
         /** this is a leaky abstraction */
         let of_raw_string: string => option(t);
         let to_raw_string: t => string;
         let compare: (t, t) => int;

         let hash: string => t;
         let verify: (~hash: t, string) => bool;

         module Magic: {
           type hash = t;
           [@deriving yojson]
           type t('a) =
             pri {
               hash,
               data: 'a,
             };

           let hash: 'a => t('a);
           let verify: (~hash: hash, 'a) => result(t('a), string);
         };
       } => {
  include P;
  include Digestif.Make_BLAKE2B({
    let digest_size = P.size;
  });

  let (let.ok) = Result.bind;

  let to_yojson = str => `String(to_hex(str));
  let of_yojson = json => {
    let.ok hex = [%of_yojson: string](json);
    try(Ok(of_hex(hex))) {
    | Invalid_argument(invalid) => Error(invalid)
    };
  };
  let of_raw_string = of_raw_string_opt;
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
      Result.bind(of_yojson(f, json), ({hash, data}) =>
        verify(~hash, data)
      );
  };
};

module BLAKE2B_20 =
  Make({
    let size = 20;
  });
include Make({
  let size = 32;
});
