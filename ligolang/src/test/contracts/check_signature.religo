let check_signature = (param : (key, signature, bytes)) : bool => {
  let pk, signed, msg = param;
  Crypto.check (pk, signed, msg);
};
