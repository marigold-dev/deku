let check_hash_key = (kh1_k2 : (key_hash, key)) : (bool, key_hash) => {
  let kh1, k2 = kh1_k2;
  let kh2 : key_hash = Crypto.hash_key (k2);
  ((kh1 == kh2), kh2)
};
