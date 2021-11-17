function check_signature (const pk     : key;
                          const signed : signature;
                          const msg    : bytes) : bool
is Crypto.check (pk, signed, msg)
