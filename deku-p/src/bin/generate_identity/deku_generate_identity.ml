open Deku_crypto

let main () =
  Mirage_crypto_rng_unix.initialize ();
  let secret = Ed25519.Secret.generate () in
  let key = Ed25519.Key.of_secret secret in
  Format.printf "Secret: %s\n%!" (Ed25519.Secret.to_b58 secret);
  Format.printf "Key: %s\n%!" (Ed25519.Key.to_b58 key)

open Cmdliner

let info =
  let doc = "Generates an Ed25519 key pair." in
  Cmd.info "deku-generate-identity" ~version:"%\226\128\140%VERSION%%" ~doc

let term =
  let open Term in
  const main $ const ()

let _ = Cmd.eval @@ Cmd.v info term
