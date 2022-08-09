(lang dune 2.9)

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
 (depends deku-stdlib deku-concepts deku-constants))

(package
 (name deku-consensus)
 (depends deku-stdlib deku-concepts deku-constants))

(package
 (name deku-chain)
 (depends deku-protocol deku-consensus))

(package
 (name deku-network)
 (depends deku-protocol deku-consensus))

(package
 (name deku-storage)
 (depends deku-crypto))

(package
 (name deku-node)
 (depends deku-protocol deku-consensus))

(package
 (name deku-bootstrap)
 (depends deku-network cmdliner))

(package
 (name deku-benchmark)
 (depends deku-protocol))

(package
 (name deku-generate-identity)
 (depends deku-crypto cmdliner))

(package (name deku-tezos))

(package (name deku-tezos-interop))

(package
 (name deku-indexer)
 (depends deku-stdlib deku-concepts deku-constants))
