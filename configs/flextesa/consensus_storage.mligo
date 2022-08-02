{
  root_hash = {
    current_block_hash = 0x;
    current_block_height = 0;
    current_state_hash = 0x;
    current_handles_hash = 0x;
    current_validators = [
        ("tz1ZaJfitdk3qEomDWgtzc54vSrrUUpvAcV9": key_hash);
        ("tz1e94qn41vgXa7fxA33j8x6WrrEJM4gihL1": key_hash);
        ("tz1ZzRtsesB972fch3VwqLi4fU4K9ALuYNui": key_hash);
        ("tz1VMbQuempdenr58JaRWVpNksqfC8dqk8v9": key_hash);
    ];
  };
  vault = {
    known_handles_hash = (Big_map.empty : vault_known_handles_hash_set);
    used_handles = (Big_map.empty : vault_used_handle_set);
    vault = (Big_map.empty : vault);
  }
}
