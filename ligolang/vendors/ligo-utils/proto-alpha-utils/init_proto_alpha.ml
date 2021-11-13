open! Memory_proto_alpha
module Signature = Tezos_base.TzPervasives.Signature
module Data_encoding = Alpha_environment.Data_encoding
module MBytes = Bytes
module Error_monad = X_error_monad
open Error_monad
open Protocol


module Context_init = struct

  type account = {
      pkh : Signature.Public_key_hash.t ;
      pk :  Signature.Public_key.t ;
      sk :  Signature.Secret_key.t ;
    }

  let generate_accounts n : (account * Alpha_context.Tez.t) list =
    let amount = Alpha_context.Tez.of_mutez_exn 4_000_000_000_000L in
    List.map ~f:(fun _ ->
        let (pkh, pk, sk) = Signature.generate_key () in
        let account = { pkh ; pk ; sk } in
        account, amount)
      (List.range 0 n)

  let make_shell
        ~level ~predecessor ~timestamp ~fitness ~operations_hash =
    Tezos_base.Block_header.{
        level ;
        predecessor ;
        timestamp ;
        fitness ;
        operations_hash ;
        (* We don't care of the following values, only the shell validates them. *)
        proto_level = 0 ;
        validation_passes = 0 ;
        context = Alpha_environment.Context_hash.zero ;
    }

  let default_proof_of_work_nonce =
    MBytes.create Alpha_context.Constants.proof_of_work_nonce_size

  let protocol_param_key = [ "protocol_parameters" ]

  let check_constants_consistency constants =
    let open Alpha_context.Constants in
    let open Error_monad in
    let { blocks_per_cycle ; blocks_per_commitment ;
          blocks_per_roll_snapshot ; _ } = constants in
    Error_monad.unless (blocks_per_commitment <= blocks_per_cycle)
      (fun () -> failwith "Inconsistent constants : blocks per commitment must be \
                           less than blocks per cycle") >>=? fun () ->
    Error_monad.unless (blocks_per_cycle >= blocks_per_roll_snapshot)
      (fun () -> failwith "Inconsistent constants : blocks per cycle \
                           must be superior than blocks per roll snapshot") >>=?
      return


  let initial_context
        constants
        header
        commitments
        initial_accounts
        security_deposit_ramp_up_cycles
        no_reward_cycles
    =
    let open Tezos_base.TzPervasives.Error_monad in
    let bootstrap_accounts =
      List.map ~f:(fun ({ pk ; pkh ; _ }, amount) ->
          Alpha_context.Parameters.{ public_key_hash = pkh ; public_key = Some pk ; amount }
        ) initial_accounts
    in
    let json =
      Data_encoding.Json.construct
        Alpha_context.Parameters.encoding
        Alpha_context.Parameters.{
          bootstrap_accounts ;
          bootstrap_contracts = [] ;
          commitments ;
          constants ;
          security_deposit_ramp_up_cycles ;
          no_reward_cycles ;
      }
    in
    let proto_params =
      Data_encoding.Binary.to_bytes_exn Data_encoding.json json
    in
    Tezos_protocol_environment.Context.(
      add Memory_context.empty ["version"] (MBytes.of_string "genesis")
    ) >>= fun ctxt ->
    Tezos_protocol_environment.Context.(
      add ctxt protocol_param_key proto_params
    ) >>= fun ctxt ->
    Main.init ctxt header
    >|= Alpha_environment.wrap_tzresult >>=? fun { context; _ } ->
    return context

  let genesis
        ?(commitments = [])
        ?(security_deposit_ramp_up_cycles = None)
        ?(no_reward_cycles = None)
        (initial_accounts : (account * Alpha_context.Tez.t) list)
    =
    if initial_accounts = [] then
      Stdlib.failwith "Must have one account with a roll to bake";

    (* Check there is at least one roll *)
    let constants : Alpha_context.Constants.parametric = Tezos_protocol_011_PtHangzH_parameters.Default_parameters.constants_test in
    check_constants_consistency constants >>=? fun () ->

    let hash =
      Alpha_environment.Block_hash.of_b58check_exn "BLockGenesisGenesisGenesisGenesisGenesisCCCCCeZiLHU"
    in
    let shell = make_shell
                  ~level:0l
                  ~predecessor:hash
                  ~timestamp:Tezos_base.TzPervasives.Time.Protocol.epoch
                  ~fitness: (Fitness_repr.from_int64 0L)
                  ~operations_hash: Alpha_environment.Operation_list_list_hash.zero in
    initial_context
      constants
      shell
      commitments
      initial_accounts
      security_deposit_ramp_up_cycles
      no_reward_cycles
    >>=? fun context ->
    return (context, shell, hash)

  let init
        ?commitments
        n =
    let open Error_monad in
    let accounts = generate_accounts n in
    let contracts = List.map ~f:(fun (a, _) ->
                        Alpha_context.Contract.implicit_contract (a.pkh)) accounts in
    genesis
      ?commitments
      accounts
    >>=? fun ctxt ->
    return (ctxt, accounts, contracts)

  let contents
        ?(proof_of_work_nonce = default_proof_of_work_nonce)
        ?(priority = 0) ?seed_nonce_hash ?(liquidity_baking_escape_vote = false) () =
    Alpha_context.Block_header.({
        priority ;
        proof_of_work_nonce ;
        seed_nonce_hash ;
        liquidity_baking_escape_vote ;
      })


  let begin_construction ?(priority=0) ~timestamp ~(header:Alpha_context.Block_header.shell_header) ~hash ctxt =
    let contents = contents ~priority () in
    let protocol_data =
      let open! Alpha_context.Block_header in {
        contents ;
        signature = Signature.zero ;
      } in
    let timestamp = Alpha_environment.Time.add timestamp @@ Int64.of_int 180 in
    Main.begin_construction
      ~chain_id: Alpha_environment.Chain_id.zero
      ~predecessor_context: ctxt
      ~predecessor_timestamp: header.timestamp
      ~predecessor_fitness: header.fitness
      ~predecessor_level: header.level
      ~predecessor:hash
      ~timestamp
      ~protocol_data
      () >>= fun x -> Lwt.return @@ Alpha_environment.wrap_tzresult x >>=? fun state ->
                      return state.ctxt

  let main n =
    init n >>=? fun ((ctxt, header, hash), accounts, contracts) ->
    let timestamp = Environment.Time.of_seconds @@ Int64.of_float @@ Unix.time () in
    begin_construction ~timestamp ~header ~hash ctxt >>=? fun ctxt ->
    return (ctxt, accounts, contracts)

end

type identity = {
    public_key_hash : Signature.public_key_hash;
    public_key : Signature.public_key;
    secret_key : Signature.secret_key;
    implicit_contract : Alpha_context.Contract.t;
  }

type environment = {
    tezos_context : Alpha_context.t ;
    identities : identity list ;
  }

let init_environment () =
  Context_init.main 10 >>=? fun (tezos_context, accounts, contracts) ->
  let accounts = List.map ~f:fst accounts in
  let x = Memory_proto_alpha.Protocol.Alpha_context.Gas.Arith.(integral_of_int_exn 800000) in
  let tezos_context = Alpha_context.Gas.set_limit tezos_context x in
  let identities =
    List.map ~f:(fun ((a:Context_init.account), c) -> {
                  public_key = a.pk ;
                  public_key_hash = a.pkh ;
                  secret_key = a.sk ;
                  implicit_contract = c ;
      }) @@
      List.zip_exn accounts contracts in
  return {tezos_context ; identities}

let contextualize ~msg ?environment f =
  let lwt =
    let environment = match environment with
      | None -> init_environment ()
      | Some x -> return x in
    environment >>=? f
  in
  force_ok ~msg @@ Lwt_main.run lwt

let dummy_environment () =
  (X_error_monad.force_lwt ~msg:"Init_proto_alpha : initing dummy environment" @@
          init_environment ())

let dummy_environment_ : environment option ref = ref None

let dummy_environment () : environment =
  match ! dummy_environment_ with
  | None ->
     let dummy_environment = dummy_environment () in
     dummy_environment_ := Some dummy_environment ;
     dummy_environment
  | Some dummy_environment -> dummy_environment
