open Operation;
open Helpers;

[@deriving yojson]
type t = {
  hash: string,
  author: Address.t,
  block_height: int64,
  main_chain_ops: list(Main_chain.t),
  side_chain_ops: list(Side_chain.Self_signed.t),
};

let make = (~author, ~block_height, ~main_chain_ops, ~side_chain_ops) => {
  let Hash.{hash, _} =
    Hash.SHA256.hash((author, block_height, main_chain_ops, side_chain_ops));
  {hash, author, block_height, main_chain_ops, side_chain_ops};
};

let of_yojson = json => {
  let.ok {hash, author, block_height, main_chain_ops, side_chain_ops} =
    of_yojson(json);
  let.ok Hash.{hash, _} =
    Hash.SHA256.verify(
      ~hash,
      (author, block_height, main_chain_ops, side_chain_ops),
    );
  Ok({hash, author, block_height, main_chain_ops, side_chain_ops});
};

let compare = (a, b) => String.compare(a.hash, b.hash);
