(lang dune 3.3)

; (name deku)

(generate_opam_files true)

(package
 (name deku-stdlib)
 (depends zarith))

(package
 (name deku-constants)
 (depends deku-stdlib))

(package
 (name deku-repr)
 (depends zarith mirage-crypto ppx_deriving ppx_yojson_conv))

(package
 (name deku-crypto)
 (depends
  ppx_deriving
  ppx_yojson_conv
  zarith
  digestif
  mirage-crypto
  mirage-crypto-ec
  mirage-crypto-rng
  secp256k1-internal))

(package
 (name deku-concepts)
 (depends deku-stdlib))

(package
 (name deku-protocol)
 (depends deku-stdlib deku-concepts deku_constants))
