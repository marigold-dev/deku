open Deku_crypto
open Deku_stdlib
open Deku_concepts
open Deku_protocol

module Parallel (D : sig
  val domains : int
end) =
struct
  let domains = D.domains
  let pool = Parallel.Pool.make ~domains
  let init_p n f = Parallel.init_p pool n f
  let filter_map_p f l = Parallel.filter_map_p pool f l
  let map_p f l = Parallel.map_p pool f l
end

module Util = struct
  let benchmark f =
    let t1 = Unix.gettimeofday () in
    let _ = f () in
    let t2 = Unix.gettimeofday () in
    t2 -. t1

  let write_to place data =
    let oc = open_out place in
    output_string oc data;
    close_out oc
end

module Block_application (D : sig
  val domains : int
end) =
struct
  module Parallel = Parallel (D)

  let generate () =
    let secret = Ed25519.Secret.generate () in
    let secret = Secret.Ed25519 secret in
    Identity.make secret

  let zero_operation ~level =
    let identity = generate () in
    let source =
      let key_hash = Identity.key_hash identity in
      Address.of_key_hash key_hash
    in
    let receiver =
      let receiver = generate () in
      let receiver = Identity.key_hash receiver in
      Address.of_key_hash receiver
    in
    let nonce = Nonce.of_n N.zero in
    let amount = Amount.of_n N.zero in
    Operation.transaction ~identity ~level ~nonce ~source ~receiver ~amount

  let payload_of_operations operations =
    (* TODO: this likely should not be here *)
    List.map
      (fun operation ->
        let json = Operation.yojson_of_t operation in
        Yojson.Safe.to_string json)
      operations

  let big_payload ~size ~level =
    let () = Format.eprintf "preparing...\n%!" in
    let operations = Parallel.init_p size (fun _ -> zero_operation ~level) in
    payload_of_operations operations

  (* TODO: parametrize over domains *)
  let perform ~protocol ~level ~payload =
    Protocol.apply ~parallel:Parallel.filter_map_p ~current_level:level ~payload
      ~tezos_operations:[] protocol

  let run ~size =
    let protocol = Protocol.initial in
    let level = Level.zero in
    let payload = big_payload ~size ~level in
    Util.benchmark (fun () -> perform ~protocol ~level ~payload)
end

module Block_production (D : sig
  val domains : int
end) =
struct
  module Parallel = Parallel (D)
  module Block_application = Block_application (D)

  let producer = Block_application.generate ()
  let sender = Block_application.generate ()

  let source =
    let key_hash = Identity.key_hash sender in
    Address.of_key_hash key_hash

  let receiver =
    Block_application.generate () |> Identity.key_hash |> Address.of_key_hash

  let operation ~nonce =
    let level = Level.zero in
    let nonce = Z.of_int nonce |> N.of_z |> Option.get |> Nonce.of_n in
    let amount = Amount.of_n N.zero in
    Operation.transaction ~identity:sender ~level ~nonce ~source ~receiver
      ~amount

  let operations ~size = Parallel.init_p size (fun nonce -> operation ~nonce)

  let produce_block ~operations =
    let open Deku_consensus in
    let level = Level.zero in
    let (Block.Block { previous; _ }) = Genesis.block in
    let _block =
      Block.produce ~parallel_map:Parallel.map_p ~identity:producer ~level
        ~previous ~operations ~tezos_operations:[]
    in
    ()

  let perform ~size =
    let () = Format.eprintf "running block production...\n%!" in
    let operations = operations ~size in
    Util.benchmark (fun () -> produce_block ~operations)

  let run ~size =
    (* FIXME: make this size configurable *)
    let average = perform ~size in
    let tx_packed_per_sec = Float.of_int size /. average in
    Format.eprintf "run time: %3f. tx packed/s: %3f\n%!" average
      tx_packed_per_sec;
    average
end

let max_domains = 7
let runs = 15
let size = 40_000

let application_domains testing ~max_domains ~runs =
  let rec go domains acc =
    let module Block_application = Block_application (struct
      let domains = domains
    end) in
    let time_list =
      List.init runs (fun _ ->
          Format.sprintf "%d,%f" domains (Block_application.run ~size))
    in
    let out = String.concat "\n" time_list in
    match domains = max_domains with
    | true -> String.concat "\n" ((testing ^ ",time") :: out :: acc)
    | false -> go (domains + 1) (out :: acc)
  in
  go 1 []

let application_transactions testing ~runs ~max_transactions =
  let rec go transactions acc =
    let module Block_application = Block_application (struct
      let domains = max_domains
    end) in
    let time_list =
      List.init runs (fun _ ->
          Format.sprintf "%d,%f" transactions
            (Block_application.run ~size:transactions))
    in
    let out = String.concat "\n" time_list in
    match transactions = max_transactions with
    | true -> String.concat "\n" ((testing ^ ",time") :: out :: acc)
    | false -> go (transactions + 5_000) (out :: acc)
  in
  go 5_000 []

let production_domains testing ~max_domains ~runs =
  let rec go domains acc =
    let module Block_production = Block_production (struct
      let domains = domains
    end) in
    let time_list =
      List.init runs (fun _ ->
          Format.sprintf "%d,%f" domains (Block_production.run ~size))
    in
    let out = String.concat "\n" time_list in
    match domains = max_domains with
    | true -> String.concat "\n" ((testing ^ ",time") :: out :: acc)
    | false -> go (domains + 1) (out :: acc)
  in
  go 1 []

let production_transactions testing ~max_transactions ~runs =
  let rec go transactions acc =
    let module Block_production = Block_production (struct
      let domains = max_domains
    end) in
    let time_list =
      List.init runs (fun _ ->
          Format.sprintf "%d,%f" transactions
            (Block_production.run ~size:transactions))
    in
    let out = String.concat "\n" time_list in
    match transactions = max_transactions with
    | true -> String.concat "\n" ((testing ^ ",time") :: out :: acc)
    | false -> go (transactions + 5_000) (out :: acc)
  in
  go 5_000 []

type 'a iterable = { initial : 'a; final : 'a; incr : 'a -> 'a }

let test_model testing ~runs ~iterable =
  let rec go value acc =
    let module Block_application = Block_application (struct
      let domains = max_domains
    end) in
    let time_list =
      List.init runs (fun _ ->
          Format.sprintf "%d,%f" value (Block_application.run ~size))
    in
    let out = String.concat "\n" time_list in
    match value >= iterable.final with
    | true -> String.concat "\n" ((testing ^ ",time") :: out :: acc)
    | false -> go (iterable.incr value) (out :: acc)
  in
  go iterable.initial []

let () =
  (* Util.write_to "application_domains"
       (application_domains "domains" ~max_domains ~runs);
     Util.write_to "application_transactions"
       (application_transactions "transactions" ~max_transactions:size ~runs);
     Util.write_to "production_domains"
       (production_domains "domains" ~max_domains ~runs); *)
  Util.write_to "production_transactions"
    (production_transactions "transactions" ~max_transactions:size ~runs)
