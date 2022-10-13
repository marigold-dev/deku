open Deku_crypto
open Cmdliner
open Common

type params = { output : string [@pos 0] [@default ""] [@docv "output"] }
[@@deriving cmdliner]

let main { output } =
  Mirage_crypto_rng_unix.initialize ();
  let priv_key = Ed25519.Secret.generate () in
  let key = Ed25519.Key.of_secret priv_key in
  let address = Ed25519.Key_hash.of_key key in
  let wallet = Wallet.make (Ed25519 address) (Ed25519 priv_key) in
  if String.equal output "" then (
    Format.printf "Secret: %s\n%!" (Ed25519.Secret.to_b58 priv_key);
    Format.printf "Key: %s\n%!" (Ed25519.Key.to_b58 key);
    Format.printf "Address: %s\n%!" (Ed25519.Key_hash.to_b58 address))
  else
    try Eio_main.run @@ fun env -> Wallet.write wallet ~env ~file:output
    with Eio.Fs.Already_exists _ ->
      Printf.eprintf "Error: file %s already exists!\n%!" output

let info =
  let doc = "Generates an Ed25519 key pair." in
  Cmd.info "generate-identity" ~version:"%\226\128\140%VERSION%%" ~doc

let term =
  let open Term in
  const main $ params_cmdliner_term ()

let cmd = Cmd.v info term
