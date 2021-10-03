opam init --disable-sandboxing --bare -y 
opam switch create . ocaml-base-compiler.4.10.2 --no-install
opam install -y dune 
eval $(opam config env)
opam install --deps-only ./ligo_wrapper.opam
