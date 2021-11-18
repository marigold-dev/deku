let main = (kh : key_hash) : contract (unit) =>
  Tezos.implicit_account (kh);
