```
bash -c "sh <(curl -fsSL https://raw.githubusercontent.com/ocaml/opam/master/shell/install.sh)" # install latest opam
opam init --disable-sandboxing --bare -y 
opam switch create . ocaml-base-compiler.4.10.2 --no-install
opam install --deps-only ./ligo_wrapper.opam
eval $(opam config env)
```
