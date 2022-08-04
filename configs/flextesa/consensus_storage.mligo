{
  root_hash = {
    current_block_hash = 0x;
    current_block_height = 0;
    current_state_hash = 0x;
    current_handles_hash = 0x;
    current_validators = [
      ("tz1fpf9DffkGAnzT6UKMDoS4hZjNmoEKhGsK": key_hash);
      ("tz1PYdVbnLwiqKo3fLFXTKxw6K7BhpddQPh8": key_hash);
      ("tz1Pv4viWq7ye4R6cr9SKR3tXiZGvpK34SKi": key_hash);
      ("tz1cXKCCxLwYCHDSrx9hfD5Qmbs4W8w2UKDw": key_hash);
    ];
  };
  vault = {
    known_handles_hash = (Big_map.empty : vault_known_handles_hash_set);
    used_handles = (Big_map.empty : vault_used_handle_set);
    vault = (Big_map.empty : vault);
  }
}
