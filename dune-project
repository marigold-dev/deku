(lang dune 3.3)

; (name deku)

(generate_opam_files true)

(package
 (name deku-stdlib)
 (depends zarith))

(package
 (name deku-crypto)
 ; TODO: I don't like this
 (depends
  deku-stdlib
  digestif
  mirage-crypto
  mirage-crypto-ec
  mirage-crypto-rng))

(package
 (name deku-concepts)
 (depends deku-stdlib))

(package
 (name deku-protocol)
 (depends deku-concepts))
