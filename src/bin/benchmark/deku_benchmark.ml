open Deku_crypto
open Deku_stdlib
open Deku_concepts
open Deku_protocol

let domains = 16
let pool = Parallel.Pool.make ~domains

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
  let default_ticket_id =
    let address =
      Deku_tezos.Contract_hash.of_string "KT1JQ5JQB4P1c8U8ACxfnodtZ4phDVMSDzgi"
      |> Option.get
    in
    let data = Bytes.of_string "" in
    Ticket_id.make address data

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
    let ticket_id = default_ticket_id in
    Operation.ticket_transfer ~identity ~level ~nonce ~receiver ~ticket_id
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
    let operations =
      Parallel.init_p pool size (fun _ -> zero_operation ~level)
    in
    payload_of_operations operations

  (* TODO: parametrize over domains *)
  let perform ~runs ~protocol ~level ~payload =
    let () = Format.eprintf "running...\n%!" in

    let (`Average average) =
      Util.benchmark ~runs (fun () ->
          let payload =
            Protocol.prepare ~parallel:(Parallel.filter_map_p pool) ~payload
          in
          let _protocol, _receipts, _errors =
            Protocol.apply ~current_level:level ~tezos_operations:[] ~payload
              protocol
          in
          ())
    in
    Format.eprintf "average: %f\n" average;
    average

  let run () =
    let protocol = Protocol.initial in
    let level = Level.zero in
    let payload = big_payload ~size:50000 ~level in
    perform ~runs:10 ~protocol ~level ~payload
end

(* TODO: enable this *)
(* module Block_production = struct
     let default_ticket_id =
       let address =
         Deku_tezos.Contract_hash.of_string "KT1JQ5JQB4P1c8U8ACxfnodtZ4phDVMSDzgi"
         |> Option.get
       in
       let data = Bytes.of_string "tuturu" in
       Ticket_id.make address data

     let producer = Zero_ops.generate ()
     let sender = Zero_ops.generate ()

     let receiver =
       Zero_ops.generate () |> Identity.key_hash |> Address.of_key_hash

     let operation ~nonce =
       let level = Level.zero in
       let nonce = Z.of_int nonce |> N.of_z |> Option.get |> Nonce.of_n in
       let ticket_id = default_ticket_id in
       let amount = Amount.of_n N.zero in
       Operation.ticket_transfer ~identity:sender ~level ~nonce ~receiver
         ~ticket_id ~amount

     let operations ~size =
       Parallel.init_p pool size (fun nonce -> operation ~nonce)

     let produce_block ~operations =
       let open Deku_consensus in
       let level = Level.zero in
       let (Block.Block { previous; _ }) = Genesis.block in
       let _block =
         Block.produce ~parallel_map:(Parallel.map_p pool) ~identity:producer
           ~level ~previous ~operations ~tezos_operations:[]
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
   end *)

let _alpha = Parallel.Pool.run pool Zero_ops.run
(* let pi = Parallel.Pool.run pool Block_production.run
   let total = alpha +. pi
   let () = Format.printf "alpha + pi: %3f\n%!" total *)
