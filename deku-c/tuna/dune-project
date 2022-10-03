(lang dune 3.4)

(cram enable)

(license MIT)

(authors "Marigold.dev")

(maintainers "Marigold.dev")

(generate_opam_files true)

(name tunac)

(package
 (name tunac)
 (synopsis "michelson to wasm compiler")
 (description "michelson to wasm compiler")
 (depends
  (ocaml
   (>= 4.13.1))
  dune
  zarith
  ppx_deriving
  ppx_yojson_conv
  yojson
  wasm
  data-encoding
  tezos-micheline
  core
  core_unix
  ppx_jane
  (ocamlformat-rpc :with-test)
  (ocamlformat :with-test)
  (ocaml-lsp-server :with-test)
  (core_bench :with-test)
  (alcotest :with-test)
  (odoc :with-doc)
  (odig :with-doc)))
