function check_hash_key (const kh1 : key_hash;
                         const k2 : key) : bool * key_hash is
  block {
    var kh2 : key_hash := Crypto.hash_key (k2);
  } with ((kh1 = kh2), kh2)
