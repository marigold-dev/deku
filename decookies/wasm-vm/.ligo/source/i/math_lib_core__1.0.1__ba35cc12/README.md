# math-lib-core

This repository provides a `Math` & `Utils` modules which export the following functions.

`Math` module
1. `isqrt`     - `nat -> nat`
2. `power`     - `nat * nat -> nat`
3. `factorial` - `nat -> nat`
4. `min`       - `nat -> nat -> nat`
5. `max`       - `nat -> nat -> nat`
6. `log_10`    - `nat -> nat`

`Utils` module
1. `Address.is_implicit`                       - `address -> bool`
2. `Bytes.Packed.is_internal_address`          - `bytes -> bool`
3. `Bytes.Packed.is_internal_address_implicit` - `bytes -> bool option`
4. `Bytes.Helpers.bytes_to_list`               - `bytes -> bytes list`
5. `Bytes.Helpers.bytes_from_list`             - `bytes list -> bytes`
6. `Bytes.Helpers.read_nb_bytes`               - `nat -> bytes -> bytes * bytes`
7. `Bytes.Conversion.bytes_to_nat`             - `bytes -> nat`


This repository is meant to provide extra features related to the native type `Bytes`.

This library introduces a `bytes_to_nat` function that allows to convert bytes to nat. 

This library introduces a `is_implicit_account` function that allows to discriminate 
KT1 addersses and tz1 addresses. This function is available on the native type 
`address` and also on native type `bytes` 
(usefull when an `address` value is packed into bytes, with the `Byte.pack` function).

### Tests

A makefile is provided to launch tests.
```
make test
```