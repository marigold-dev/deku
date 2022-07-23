open Deku_crypto
open Deku_concepts
open Deku_protocol
open Deku_stdlib

let current_level = Level.of_n N.zero
let secret = Secret.Ed25519 (Ed25519.Secret.generate ())
let key = Key.of_secret secret
let key_hash = Key_hash.of_key key
let address = Address.of_key_hash key_hash
let n = 50_000

let measure f =
  let start_time = Unix.gettimeofday () in
  let result = f () in
  let end_time = Unix.gettimeofday () in
  (end_time -. start_time, result)

let make_payload () =
  List.init n (fun i ->
      let nonce = i |> Z.of_int |> N.of_z |> Option.get |> Nonce.of_n in
      (* FIXME: this is kind of annoying *)
      let (Operation op as operation) =
        Operation.transaction ~level:current_level ~nonce ~source:address
          ~receiver:address ~amount:Amount.zero
      in
      let signature =
        Signature.sign secret (Operation_hash.to_blake2b op.hash)
      in
      (key, signature, operation))

let payload =
  let elapsed_time, payload = measure make_payload in
  Format.printf "Constructed %d operations in %3f seconds\n%!" n elapsed_time;
  payload

let benchmark () =
  let elapsed_time, _protocol' =
    measure (fun () -> Protocol.apply ~current_level ~payload Protocol.initial)
  in
  let tps = Float.of_int n /. elapsed_time in
  Format.printf "Applied %d ops in %3f seconds. TPS: %3f\n%!" n elapsed_time tps;
  ()

let main () = List.init 5 Fun.id |> List.iter (fun _ -> benchmark ())
let () = main ()
