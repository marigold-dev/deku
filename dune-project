(lang dune 3.3)

; (name deku)

(generate_opam_files true)

(package
 (name deku-stdlib)
 (depends zarith))

(package
 (name deku-crypto)
 (depends
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
 (depends deku-concepts))
