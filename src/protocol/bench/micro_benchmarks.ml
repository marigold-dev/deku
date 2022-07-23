open Core
open Core_bench
open Deku_stdlib
open Deku_crypto
open Deku_concepts
open Deku_protocol

let benchmarks =
  [
    ( "Operation.verify",
      let level = Level.of_n N.zero in
      let nonce = Nonce.of_n N.zero in
      let secret = Secret.Ed25519 (Ed25519.Secret.generate ()) in
      let key = Key.of_secret secret in
      let key_hash = Key_hash.of_key key in
      let address = Address.of_key_hash key_hash in
      let (Operation op as operation) =
        Operation.transaction ~level ~nonce ~source:address ~receiver:address
          ~amount:Amount.zero
      in
      let signature =
        Signature.sign secret (Operation_hash.to_blake2b op.hash)
      in
      fun () ->
        let _valid = Operation.verify key signature operation in
        () );
  ]

let main () =
  match benchmarks with
  | [ (_, f) ] -> List.init ~f:Fun.id 100_000 |> List.iter ~f:(fun _ -> f ())
  | _ -> assert false

let () = main ()

(* TODO:  We probably want to include core_bench *and* profiling with landmarks.
   Maybe 2 modules: profiling.ml and micro_benchmarks.ml that share functions *)
(* let () =
   List.map benchmarks ~f:(fun (name, test) -> Bench.Test.create ~name test)
   |> Bench.make_command |> Command_unix.run *)
