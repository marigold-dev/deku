open Deku_stdlib
open Deku_concepts
open Deku_protocol
open Deku_ledger
open Deku_crypto
open Deku_consensus

let identity =
  let secret = Ed25519.Secret.generate () in
  let secret = Secret.Ed25519 secret in
  Identity.make secret

let block ~default_block_size =
  let above = Genesis.block in
  let withdrawal_handles_hash = BLAKE2b.hash "potato" in
  let producer = Producer.empty in
  Producer.produce ~identity ~default_block_size ~above ~withdrawal_handles_hash
    producer

module type BENCH = sig
  type t

  val name : string
  val item_message : string
  val items : int
  val prepare : unit -> t
  val run : t -> unit
end

module Produce : BENCH = struct
  type t = unit

  let items = 1_000_000
  let name = "produce"
  let item_message = Format.sprintf "%d" items
  let prepare () = ()
  (* 6kk/s on 8 domains *)

  let run () =
    let (_ : Block.t) = block ~default_block_size:items in
    ()
end

module Prepare_and_decode : BENCH = struct
  type t = string

  let name = "prepare_and_decode"
  let items = 100_000
  let item_message = Format.sprintf "%d" items

  let prepare () =
    let (Block.Block { payload; _ }) = block ~default_block_size:items in
    payload

  let parallel = Parallel.filter_map_p

  (* 100k/s on 8 domains *)
  let run payload =
    let (Payload payload) = Payload.decode ~payload in
    let (_ : Operation.Initial.t list) = Protocol.prepare ~parallel ~payload in
    ()
end

module Produce_and_serialize : BENCH = struct
  type t = unit

  let name = "produce_and_serialize"
  let items = 1_000_000
  let item_message = Format.sprintf "%d" items
  let prepare () = ()

  let run () =
    let block = block ~default_block_size:items in
    let (_ : string) =
      Data_encoding.Binary.to_string_exn Block.encoding block
    in
    ()
end

module Produce_and_serialize_and_parse : BENCH = struct
  type t = unit

  let name = "produce_and_serialize_and_parse"
  let items = 1_000_000
  let item_message = Format.sprintf "%d" items
  let prepare () = ()

  let run () =
    let block = block ~default_block_size:items in
    let (binary : string) =
      Data_encoding.Binary.to_string_exn Block.encoding block
    in
    let (_ : Block.t) =
      Data_encoding.Binary.of_string_exn Block.encoding binary
    in
    ()
end

module Verify : BENCH = struct
  type t = Key.key * (BLAKE2b.t * Signature.signature) list

  let name = "verify"
  let items = 100_000
  let item_message = Format.sprintf "%d" items

  let prepare () =
    let identity =
      let secret = Ed25519.Secret.generate () in
      let secret = Secret.Ed25519 secret in
      Identity.make secret
    in
    let key = Identity.key identity in
    let items =
      Parallel.init_p items (fun n ->
          let string = string_of_int n in
          let hash = BLAKE2b.hash string in
          let sign = Identity.sign ~hash identity in
          (hash, sign))
    in
    (key, items)

  let run (key, items) =
    let _units : unit list =
      Parallel.map_p
        (fun (hash, sign) -> assert (Signature.verify key sign hash))
        items
    in
    ()
end

module Ledger_balance = struct
  type t =
    Ledger.ledger * (Address.address * Amount.amount * Ticket_id.ticket_id) list

  let name = "ledger_balance"
  let items = 100_000
  let item_message = Format.sprintf "%d" items

  let address () =
    let secret = Ed25519.Secret.generate () in
    let secret = Secret.Ed25519 secret in
    let identity = Identity.make secret in
    Address.of_key_hash (Identity.key_hash identity)

  let prepare () =
    let items =
      Parallel.init_p items (fun n ->
          let sender = address () in
          let amount =
            let z = Z.of_int n in
            let n = Option.get (N.of_z z) in
            Amount.of_n n
          in
          let ticket_id =
            let ticketer =
              let open Deku_tezos in
              Option.get
                (Contract_hash.of_b58 "KT1HbQepzV1nVGg8QVznG7z4RcHseD5kwqBn")
            in
            Ticket_id.make (Deku_ledger.Ticket_id.Tezos ticketer)
              (Bytes.make 0 '\000')
          in
          (sender, amount, ticket_id))
    in
    let ledger =
      List.fold_left
        (fun ledger (sender, amount, ticket_id) ->
          Ledger.deposit sender amount ticket_id ledger)
        Ledger.initial items
    in
    (ledger, items)

  let run (ledger, items) =
    let (_ : unit) =
      List.iter
        (fun (sender, amount, ticket_id) ->
          let balance = Ledger.balance sender ticket_id ledger in
          (* TODO: this is a >= because of rng collision *)
          assert (balance >= amount))
        items
    in
    ()
end

module Ledger_transfer : BENCH = struct
  type t =
    Ledger.ledger
    * (Address.address * Address.address * Amount.amount * Ticket_id.ticket_id)
      list

  let name = "ledger_transfer"
  let items = 100_000
  let item_message = Format.sprintf "%d" items

  let address () =
    let secret = Ed25519.Secret.generate () in
    let secret = Secret.Ed25519 secret in
    let identity = Identity.make secret in
    Address.of_key_hash (Identity.key_hash identity)

  let prepare () =
    let items =
      Parallel.init_p items (fun n ->
          let sender = address () in
          let receiver = address () in
          let amount =
            let z = Z.of_int n in
            let n = Option.get (N.of_z z) in
            Amount.of_n n
          in
          let ticket_id =
            let ticketer =
              let open Deku_tezos in
              Option.get
                (Contract_hash.of_b58 "KT1HbQepzV1nVGg8QVznG7z4RcHseD5kwqBn")
            in
            Ticket_id.make (Tezos ticketer) (Bytes.make 0 '\000')
          in
          (sender, receiver, amount, ticket_id))
    in
    let ledger =
      List.fold_left
        (fun ledger (sender, _receiver, amount, ticket_id) ->
          Ledger.deposit sender amount ticket_id ledger)
        Ledger.initial items
    in
    (ledger, items)

  let run (ledger, items) =
    let (_ : Ledger.t) =
      List.fold_left
        (fun ledger (sender, receiver, amount, ticket_id) ->
          Result.get_ok
            (Ledger.transfer ~sender ~receiver ~amount ~ticket_id ledger))
        ledger items
    in
    ()
end

let bench f =
  let t1 = Unix.gettimeofday () in
  let () = f () in
  let t2 = Unix.gettimeofday () in
  t2 -. t1

let log_item ~name ~item_message ~data =
  Format.printf "%s, %s, %f\n%!" name item_message data

let run_benchmark (module Bench : BENCH) =
  let open Bench in
  let runs = 10 in
  let value = prepare () in
  let data =
    List.init runs (fun _ ->
        let time = bench (fun () -> run value) in
        log_item ~name ~item_message ~data:time;
        time)
  in
  let total = List.fold_left (fun acc time -> acc +. time) 0.0 data in
  let average = total /. Int.to_float runs in
  let per_second = Int.to_float items /. average in
  Format.eprintf "%s: %.3f/s\n%!" name per_second;
  ()

let benches : (module BENCH) list =
  [
    (module Produce);
    (module Prepare_and_decode);
    (module Produce_and_serialize);
    (module Produce_and_serialize_and_parse);
    (module Verify);
    (module Ledger_balance);
    (module Ledger_transfer);
  ]

type params = { domains : int [@env "DEKU_DOMAINS"] [@default 16] }
[@@deriving cmdliner]

let main params =
  let { domains } = params in
  Eio_main.run @@ fun env ->
  Format.eprintf "running with %d domains\n%!" domains;
  Format.printf "benchmark, items, duration\n%!";
  Parallel.Pool.run ~env ~domains @@ fun () ->
  List.iter (fun (module Bench : BENCH) -> run_benchmark (module Bench)) benches

let () =
  let open Cmdliner in
  let info = Cmd.info "deku-benchmark" in
  let term = Term.(const main $ params_cmdliner_term ()) in
  let cmd = Cmd.v info term in
  exit (Cmd.eval ~catch:false cmd)
