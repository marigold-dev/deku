(* This script will generate all the data used in the Alg_intf tests

   Build in a project with tezos-crypto with the following dune file:
   (executable
    (name data_gen)
    (libraries tezos_crypto)
    (preprocess
     (pps)))
*)

open Tezos_crypto

type ed25519_id = {
  public_key_hash : Ed25519.Public_key_hash.t;
  public_key : Ed25519.Public_key.t;
  secret_key : Ed25519.Secret_key.t;
}

let index = Atomic.make 0

let get_incr () =
  Atomic.incr index;
  Atomic.get index

let get_ed25519_id () =
  let open Ed25519 in
  let a, b, c = generate_key () in
  Format.printf
    "let e%d = {public_key_hash = Public_key_hash.of_b58check_exn \"%s\";\n\
    \    public_key = Public_key.of_b58check_exn \"%s\";\n\
    \    secret_key = Secret_key.of_b58check_exn \"%s\";}\n\
     %!"
    (get_incr ())
    (Public_key_hash.to_b58check a)
    (Public_key.to_b58check b) (Secret_key.to_b58check c)

let spc () = Format.eprintf "\n%!"

let generate_identities () =
  get_ed25519_id ();
  spc ();
  get_ed25519_id ();
  spc ();
  get_ed25519_id ();
  spc ();
  get_ed25519_id ();
  spc ();
  get_ed25519_id ()

module Data_gen (Crypto : sig
  include S.SIGNATURE
  include S.RAW_DATA with type t := t

  type id = {
    public_key_hash : Public_key_hash.t;
    public_key : Public_key.t;
    secret_key : Secret_key.t;
  }

  val ids : id list
end) =
struct
  open Crypto

  module Skt_key = struct
    let secret_keys = List.map (fun id -> id.secret_key) ids

    let public_keys =
      List.map (fun sk -> Secret_key.to_public_key sk) secret_keys

    let compare_secret_keys = List.sort Secret_key.compare secret_keys

    let equality_secret_keys =
      List.for_all (fun sk -> Secret_key.equal sk sk) secret_keys
  end

  module Print_secret_key = struct
    let print_public_keys () =
      Format.printf "let public_keys = [\n%!";
      List.iter
        (fun pk -> Format.printf "\"%s\";\n%!" (Public_key.to_b58check pk))
        Skt_key.public_keys;
      Format.printf "]\n%!"

    let print_compare_secret_keys () =
      Format.printf "let compared_secret_keys = [\n%!";
      List.iter
        (fun sk -> Format.printf "\"%s\"\n%!;" (Secret_key.to_b58check sk))
        Skt_key.compare_secret_keys;
      Format.printf "]\n%!"

    let print_equality_secret_keys () =
      Format.printf "let equality_secret_keys = %b\n%!"
        Skt_key.equality_secret_keys
  end
end
