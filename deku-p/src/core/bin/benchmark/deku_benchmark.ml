open Deku_stdlib
open Deku_concepts
open Deku_protocol
open Deku_ledger
open Deku_crypto
open Deku_consensus

let domains =
  match Sys.getenv_opt "DEKU_DOMAINS" with
  | Some domains -> int_of_string domains
  | None -> 16

let () = Format.printf "Using %d domains\n%!" domains

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

let bench f =
  let t1 = Unix.gettimeofday () in
  let () = f () in
  let t2 = Unix.gettimeofday () in
  t2 -. t1

let formatter ~name ~item_message ~data =
  Format.sprintf "%s with %s: %.3f\n" name item_message data

let writer ~data = Format.eprintf "%s%!" data

let bench ~name ~item_message ~prepare run (formatter, writer) =
  let runs = 10 in
  let value = prepare () in
  let _ =
    List.init runs (fun _ ->
        let time = bench (fun () -> run value) in
        let data = formatter ~name ~item_message ~data:time in
        writer ~data)
  in
  ()

module type Bench = sig
  type t

  val name : string
  val item_message : string
  val items : int
  val prepare : unit -> t
  val run : t -> unit
end

let bench (module Bench : Bench) writer =
  let open Bench in
  let _ = bench ~name ~item_message ~prepare run writer in
  ()

let produce writer () =
  let module Bench : Bench = struct
    type t = unit

    let items = 1_000_000
    let name = "produce"
    let item_message = Format.sprintf "%d" items
    let prepare () = ()
    (* 6kk/s on 8 domains *)

    let run () =
      let (_ : Block.t) = block ~default_block_size:items in
      ()
  end in
  bench (module Bench) writer

let block_encode writer () =
  let module Bench = struct
    type t = Block.block

    let name = "block_encode"
    let items = 1_000_000
    let prepare () = block ~default_block_size:items
    let item_message = Format.sprintf "%d" items

    let run block =
      let (_ : string list) = Block.encode block in
      ()
  end in
  bench (module Bench) writer

let block_decode writer () =
  let module Bench = struct
    type t = string list

    let name = "block_decode"
    let items = 1_000_000
    let item_message = Format.sprintf "block size of %d" items

    let prepare () =
      let block = block ~default_block_size:items in
      Block.encode block

    let run fragments =
      let (_ : Block.t) = Block.decode fragments in
      ()
  end in
  bench (module Bench) writer

let prepare_and_decode writer () =
  let module Bench = struct
    type t = string

    let name = "prepare_and_decode"
    let items = 100_000
    let item_message = Format.sprintf "block size of %d" items

    let prepare () =
      let (Block.Block { payload; _ }) = block ~default_block_size:items in
      payload

    let parallel = Parallel.filter_map_p

    (* 100k/s on 8 domains *)
    let run payload =
      let (Payload payload) = Payload.decode ~payload in
      let (_ : Operation.Initial.t list) =
        Protocol.prepare ~parallel ~payload
      in
      ()
  end in
  bench (module Bench) writer

let verify writer () =
  let module Bench = struct
    type t = Key.key * (BLAKE2b.t * Signature.signature) list

    let name = "verify"
    let items = 100_000
    let item_message = Format.sprintf "%d tickets" items

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
  end in
  bench (module Bench) writer

let ledger_balance writer () =
  let module Bench = struct
    type t =
      Ledger.ledger
      * (Address.address * Amount.amount * Ticket_id.ticket_id) list

    let name = "ledger_balance"
    let items = 100_000
    let item_message = Format.sprintf "%d tickets" items

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
  end in
  bench (module Bench) writer

let ledger_transfer writer () =
  let module Bench = struct
    type t =
      Ledger.ledger
      * (Address.address
        * Address.address
        * Amount.amount
        * Ticket_id.ticket_id)
        list

    let name = "ledger_transfer"
    let items = 100_000
    let item_message = Format.sprintf "%d transfers" items

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
  end in
  bench (module Bench) writer

let benches =
  [
    produce;
    block_encode;
    block_decode;
    prepare_and_decode;
    verify;
    ledger_balance;
    ledger_transfer;
  ]

let () =
  Eio_main.run @@ fun env ->
  Parallel.Pool.run ~env ~domains @@ fun () ->
  List.iter (fun bench -> bench (formatter, writer) ()) benches
