open Steps

let rec dispatch effect step state =
  match step with
  | Noop -> (state, [])
  | Effect eff ->
    let () = effect eff state in
    (state, [])
  | Both (left, right) ->
    let state, err_left = dispatch effect left state in
    let state, err_right = dispatch effect right state in
    (state, err_left @ err_right)
  | Check_operation { operation } -> check_operation ~operation effect state
  | Append_operation { operation } -> append_operation ~operation effect state
  | Check_block { block } -> check_block ~block effect state
  | Append_block { block } -> append_block ~block effect state
  | Check_signature { hash; signature } ->
    check_signature ~hash ~signature effect state
  | Append_signature { hash; signature } ->
    append_signature ~hash ~signature effect state
  | Is_signed_block { hash } -> is_signed_block ~hash effect state
  | Is_future_block { signed; block } ->
    is_future_block ~signed ~block effect state
  | Is_signable_block { block } -> is_signable_block ~block effect state
  | Sign_block { block } -> sign_block ~block effect state
  | Pre_apply_block { block; signatures } ->
    pre_apply_block ~block ~signatures effect state
  | Apply_block_header { block } -> apply_block_header ~block effect state
  | Apply_block_data { block; previous_protocol } ->
    apply_block_data ~previous_protocol ~block effect state
  | Post_apply_block -> post_apply_block effect state
  | Can_produce_block -> can_produce_block effect state
  | Produce_block -> produce_block effect state
  | Check_validator_change { payload; signature } ->
    check_validator_change ~payload ~signature effect state
  | Allow_to_add_validator { key_hash } ->
    allow_to_add_validator ~key_hash effect state
  | Allow_to_remove_validator { key_hash } ->
    allow_to_remove_validator ~key_hash effect state

and check_operation ~operation effect state =
  let step = Steps.check_operation ~operation state in
  dispatch effect step state

and append_operation ~operation effect state =
  let state, step = Steps.append_operation ~operation state in
  dispatch effect step state

and check_block ~block effect state =
  match Steps.check_block ~block state with
  | Ok step -> dispatch effect step state
  | Error err -> (state, [err])

and append_block ~block effect state =
  let state, step = Steps.append_block ~block state in
  dispatch effect step state

and check_signature ~hash ~signature effect state =
  match Steps.check_signature ~hash ~signature state with
  | Ok step -> dispatch effect step state
  | Error err -> (state, [err])

and append_signature ~hash ~signature effect state =
  let state, step = Steps.append_signature ~hash ~signature state in
  dispatch effect step state

and is_signed_block ~hash effect state =
  let step = Steps.is_signed_block ~hash state in
  dispatch effect step state

and is_future_block ~signed ~block effect state =
  let step = Steps.is_future_block ~signed ~block state in
  dispatch effect step state

and is_signable_block ~block effect state =
  let step = Steps.is_signable_block ~block state in
  dispatch effect step state

and sign_block ~block effect state =
  let step = Steps.sign_block ~block state in
  dispatch effect step state

and pre_apply_block ~block ~signatures effect state =
  let state, step = Steps.pre_apply_block ~block ~signatures state in
  dispatch effect step state

and apply_block_header ~block effect state =
  match Apply_block.apply_block_header ~block state with
  | Ok (state, step) -> dispatch effect step state
  | Error err -> (state, [err])

and apply_block_data ~previous_protocol ~block effect state =
  match Apply_block.apply_block_data ~previous_protocol ~block state with
  | Ok (state, _snapshot_ref, step) -> dispatch effect step state
  | Error err -> (state, [err])

and post_apply_block effect state =
  let step = Steps.post_apply_block state in
  dispatch effect step state

and can_produce_block effect state =
  let step = Steps.can_produce_block state in
  dispatch effect step state

and produce_block effect state =
  let step = Produce_block.produce_block state in
  dispatch effect step state

and check_validator_change ~payload ~signature effect state =
  match Steps.check_validator_change ~payload ~signature state with
  | Ok step -> dispatch effect step state
  | Error err -> (state, [err])

and allow_to_add_validator ~key_hash effect state =
  let state, step = Steps.allow_to_add_validator ~key_hash state in
  dispatch effect step state

and allow_to_remove_validator ~key_hash effect state =
  let state, step = Steps.allow_to_remove_validator ~key_hash state in
  dispatch effect step state
