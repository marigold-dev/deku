open Operation;
open Helpers;

[@deriving yojson]
type t = {
  hash: string,
  previous_hash: string,
  author: Address.t,
  block_height: int64,
  main_chain_ops: list(Main_chain.t),
  side_chain_ops: list(Side_chain.Self_signed.t),
};

let to_yojson = t => {
  let to_hex = str => {
    let `Hex(str) = Hex.of_string(str);
    str;
  };
  to_yojson({
    ...t,
    hash: to_hex(t.hash),
    previous_hash: to_hex(t.previous_hash),
  });
};

let of_yojson = str => {
  let of_hex = str => Hex.to_string(`Hex(str));
  // TODO: validate this hash
  of_yojson(str)
  |> Result.map(t =>
       {...t, hash: of_hex(t.hash), previous_hash: of_hex(t.previous_hash)}
     );
};

let make =
    (~previous_hash, ~author, ~block_height, ~main_chain_ops, ~side_chain_ops) => {
  let Hash.{hash, _} =
    Hash.SHA256.hash((
      previous_hash,
      author,
      block_height,
      main_chain_ops,
      side_chain_ops,
    ));
  {hash, previous_hash, author, block_height, main_chain_ops, side_chain_ops};
};

let of_yojson = json => {
  let.ok {
    hash,
    previous_hash,
    author,
    block_height,
    main_chain_ops,
    side_chain_ops,
  } =
    of_yojson(json);
  let.ok Hash.{hash, _} =
    Hash.SHA256.verify(
      ~hash,
      (previous_hash, author, block_height, main_chain_ops, side_chain_ops),
    );
  Ok({
    hash,
    previous_hash,
    author,
    block_height,
    main_chain_ops,
    side_chain_ops,
  });
};

let compare = (a, b) => String.compare(a.hash, b.hash);
