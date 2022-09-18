open Deku_crypto
open Deku_stdlib
open Deku_concepts
open Deku_protocol

module Parallel = struct
  let domains = 16
  let pool = Parallel.Pool.make ~domains
  let init_p n f = Parallel.init_p pool n f
  let filter_map_p f l = Parallel.filter_map_p pool f l
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

module Zero_ops = struct
  let generate () =
    let secret = Ed25519.Secret.generate () in
    let secret = Secret.Ed25519 secret in
    Identity.make secret

  let zero_operation ~level =
    let identity = generate () in
    let receiver =
      let receiver = generate () in
      let receiver = Identity.key_hash receiver in
      Address.of_key_hash receiver
    in
    let nonce = Nonce.of_n N.zero in
    let amount = Amount.of_n N.zero in
    Operation.transaction ~identity ~level ~nonce ~receiver ~amount

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
    let () = Format.eprintf "running...\n%!" in

    let (`Average average) =
      Util.benchmark ~runs (fun () ->
          let _protocol, _receipts =
            Protocol.apply ~parallel:Parallel.filter_map_p ~current_level:level
              ~payload protocol
          in
          ())
    in
    Format.eprintf "average: %f\n" average

  let run () =
    let protocol = Protocol.initial in
    let level = Level.zero in
    let payload = big_payload ~size:50000 ~level in
    perform ~runs:10 ~protocol ~level ~payload
end

let () = Zero_ops.run ()
