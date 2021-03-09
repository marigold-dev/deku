open Operation;

[@deriving (yojson, ord)]
type t =
  pri {
    hash: string,
    author: Address.t,
    block_height: int64,
    main_chain_ops: list(Main_chain.t),
    side_chain_ops: list(Side_chain.Self_signed.t),
  };

let make:
  (
    ~author: Address.t,
    ~block_height: int64,
    ~main_chain_ops: list(Main_chain.t),
    ~side_chain_ops: list(Side_chain.Self_signed.t)
  ) =>
  t;
