let (let.ok) = Result.bind;
let (let.some) = Option.bind;

// TODO: this is bad, size shuold be a detail in a function
module Make =
       (P: {let size: int;})
       : {
         [@deriving yojson]
         type t;
         let to_string: t => string;
         let of_string: string => option(t);
         /** this is a leaky abstraction */
         let of_raw_string: string => option(t);
         let to_raw_string: t => string;
         let compare: (t, t) => int;

         let hash: string => t;
         let verify: (~hash: t, string) => bool;

         let both: (t, t) => t;
       } => {
  include P;
  include Digestif.Make_BLAKE2B({
    let digest_size = P.size;
  });

  let of_raw_string = of_raw_string_opt;
  let to_string = to_hex;
  let of_string = string => {
    let.some () = String.length(string) == size * 2 ? Some() : None;
    of_hex_opt(string);
  };
  let to_yojson = str => `String(to_hex(str));
  let of_yojson = json => {
    let.ok hex = [%of_yojson: string](json);
    of_string(hex) |> Option.to_result(~none="Invalid hex");
  };
  // TODO: check the unsafe here
  let compare = unsafe_compare;

  let hash = data => digest_string(data);
  let verify = (~hash as expected_hash, data) => expected_hash == hash(data);

  let both = (a, b) => hash(to_raw_string(a) ++ to_raw_string(b));
};

module BLAKE2B_20 =
  Make({
    let size = 20;
  });
include Make({
  let size = 32;
});
