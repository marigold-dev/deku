function main (const p : key_hash) : address is block {
  const c : contract (unit) = Tezos.implicit_account (p);
} with Tezos.address (c)
