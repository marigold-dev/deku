open Operation;

[@deriving (yojson, ord)]
type t = {
  author: Address.t,
  block_height: int64,
  main_chain_ops: list(Main_chain.t),
  side_chain_ops: list(Side_chain.Self_signed.t),
};

module Self_signed =
  Signed.Make({
    type nonrec t = t;
    let compare = compare;
    let to_yojson = to_yojson;
    let of_yojson = of_yojson;
    let verify = (~key, ~signature as _, data) => key == data.author;
  });
