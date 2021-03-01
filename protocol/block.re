open Operation;

[@deriving yojson]
type t = {
  block_height: int64,
  main_chain_ops: list(Main_chain.t),
  side_chain_ops: list(Signed.t(Side_chain.t)),
};
