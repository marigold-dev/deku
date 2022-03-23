# Build examples

## Generate 

To generate the Lambda_VM in OCaml to Coq by using the tool [**coq-of-ocaml**](https://github.com/foobar-land/coq-of-ocaml) do:

`ruby generate.rb path_1 path_2`

where:

- `path_1` is the path of deku 
- `path_2` is the path of OCaml Lambda_VM 

This command does 2 things:
- Use `coq-of-ocaml` to generate the file(s) `*.ml` in Lambda_VM to `*.v`. The generated Coq files is stored in the folder `deku/lambda_vm_coq`.
- Create the `_CoqProject` and `Makefile` 

For example:

`ruby generate.rb ~/deku src/lambda_vm`

Finally, to compile the `Coq` files do: `make`