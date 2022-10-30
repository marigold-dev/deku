open Deku_stdlib
open Deku_crypto
open Deku_indexer
open Deku_protocol
open Deku_concepts
open Deku_tezos

let domains =
  match Sys.getenv_opt "DEKU_DOMAINS" with
  | Some domains -> int_of_string domains
  | None -> 16

let () = Format.printf "Using %d domains\n%!" domains

(* FIXME: why is this needed? *)
let () = Mirage_crypto_rng_unix.initialize ()

let identities =
  List.init 1000 (fun _ ->
      let secret = Ed25519.Secret.generate () in
      let secret = Secret.Ed25519 secret in
      Identity.make secret)

let ticket_id =
  let data = Bytes.of_string "hello world" in

  (* A random contract address *)
  let ticketer =
    Deku_ledger.Ticket_id.Tezos
      (Deku_tezos.Contract_hash.of_b58 "KT1WvzYHCNBvDSdwafTHv7nJ1dWmZ8GCYuuC"
      |> Option.get)
  in
  Deku_ledger.Ticket_id.make ticketer data

let tezos_ticet_id =
  Deku_ledger.Ticket_id.to_tezos_ticket ticket_id |> Option.get

let protocol, _, _ =
  let internal_tezos_operations =
    List.map
      (fun identity ->
        let key_hash = Identity.key_hash identity in
        let amount = Z.of_int 100 |> N.of_z |> Option.get |> Amount.of_n in
        Tezos_operation.Deposit
          { destination = key_hash; amount; ticket = tezos_ticet_id })
      identities
  in
  (* Some random Tezos operation hash *)
  let hash =
    Tezos_operation_hash.of_b58
      "oo3HjzvHJZ5aWizN8ZW24Hp63MzuGuPCpUH4MXzWy2JZJrnRPJx"
    |> Option.get
  in
  let tezos_operation = Tezos_operation.make hash internal_tezos_operations in
  let current_level = Level.next Level.zero in
  Protocol.apply ~current_level ~payload:[]
    ~tezos_operations:[ tezos_operation ] Protocol.initial

let operations =
  List.map
    (fun identity ->
      let key_hash = Identity.key_hash identity in
      let nonce = Nonce.of_n N.one in
      let level = Level.zero in
      let tezos_owner = Address.Implicit key_hash in
      let amount = Amount.one in
      let (Signed_operation { initial; _ }) =
        Operation.Signed.withdraw ~identity ~nonce ~level ~tezos_owner
          ~ticket_id ~amount
      in
      initial)
    identities

let main () =
  Eio_main.run @@ fun _env ->
  (* Run a local postgres, e.g:
     docker run --rm -P -p 127.0.0.1:5432:5432 -e POSTGRES_PASSWORD="1234" --name deku-postgres postgres *)
  let indexer =
    Indexer.make
      ~uri:(Uri.of_string "postgresql://postgres:1234@localhost:5432/postgres")
  in
  Format.printf "Applying %d withdraws\n%!" (List.length operations);
  let current_level = Level.zero |> Level.next |> Level.next in
  let time = Unix.gettimeofday () in
  let protocol, receipts, _ =
    Protocol.apply ~current_level ~payload:operations ~tezos_operations:[]
      protocol
  in
  Format.printf "Finished applying operations in %3f sec\n%!"
    (Unix.gettimeofday () -. time);
  Format.printf "Updating database\n%!";
  let time = Unix.gettimeofday () in
  Indexer.on_block ~state:protocol ~operations ~receipts indexer;
  let delta = Unix.gettimeofday () -. time in
  Format.printf "Finished applying operations in %3f sec\n%!" delta;
  Format.printf "TPS: %3f\n%!"
    ((Float.of_int @@ List.length operations) /. delta)

let () =
  Eio_main.run @@ fun env ->
  Parallel.Pool.run ~env ~domains @@ fun () -> main ()
