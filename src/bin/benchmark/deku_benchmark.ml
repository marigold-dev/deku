open Deku_crypto
open Deku_stdlib
open Deku_concepts
open Deku_protocol

module Parallel = struct
  let domains = 12
  let pool = Parallel.Pool.make ~domains
  let init_p n f = Parallel.init_p pool n f
  let filter_map_p f l = Parallel.filter_map_p pool f l
  let map_p f l = Parallel.map_p pool f l
end

module Util = struct
  let benchmark ~runs f =
    let times =
      List.init runs (fun _n ->
          let t1 = Unix.gettimeofday () in
          let () = f () in
          let t2 = Unix.gettimeofday () in
          t2 -. t1)
    in
    let total = List.fold_left (fun total time -> total +. time) 0.0 times in
    let average = total /. Float.of_int runs in
    `Average average
end

module Block_application = struct
  let default_ticket_id =
    let address =
      Deku_tezos.Contract_hash.of_string "KT1JQ5JQB4P1c8U8ACxfnodtZ4phDVMSDzgi"
      |> Option.get
    in
    let data = Bytes.of_string "tuturu" in
    Ticket_id.make address data

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
    let ticket_id = default_ticket_id in
    Operation.transaction ~identity ~level ~nonce ~source ~receiver ~ticket_id
      ~amount

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
  let perform ~runs ~protocol ~level ~payload =
    let () = Format.eprintf "running block application...\n%!" in

    let (`Average average) =
      Util.benchmark ~runs (fun () ->
          let _protocol, _receipts =
            Protocol.apply ~parallel:Parallel.filter_map_p ~current_level:level
              ~payload ~tezos_operations:[] protocol
          in
          ())
    in
    average

  let run () =
    let protocol = Protocol.initial in
    let level = Level.zero in
    let size = 50000 in
    let payload = big_payload ~size ~level in
    let payload_size =
      List.fold_left (fun a s -> a + String.length s) 0 payload
    in
    Format.printf "Block size: %d\n%!" payload_size;
    let average = perform ~runs:10 ~protocol ~level ~payload in
    let tps = Float.of_int size /. average in
    Format.eprintf "average run time: %3f. tps: %3f\n%!" average tps;
    average
end

module Block_production = struct
  let default_ticket_id =
    let address =
      Deku_tezos.Contract_hash.of_string "KT1JQ5JQB4P1c8U8ACxfnodtZ4phDVMSDzgi"
      |> Option.get
    in
    let data = Bytes.of_string "tuturu" in
    Ticket_id.make address data

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
    let ticket_id = default_ticket_id in
    let amount = Amount.of_n N.zero in
    Operation.transaction ~identity:sender ~level ~nonce ~source ~receiver
      ~ticket_id ~amount

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

  let perform ~runs ~size =
    let () = Format.eprintf "running block production...\n%!" in
    let operations = operations ~size in
    let (`Average average) =
      Util.benchmark ~runs (fun () -> produce_block ~operations)
    in
    average

  let run () =
    let size = 50000 in
    let average = perform ~runs:5 ~size in
    let tx_packed_per_sec = Float.of_int size /. average in
    Format.eprintf "average run time: %3f. tx packed/s: %3f\n%!" average
      tx_packed_per_sec;
    average
end

let alpha = Block_application.run ()
let pi = Block_production.run ()
let total = alpha +. pi
let () = Format.printf "alpha + pi: %3f\n%!" total
