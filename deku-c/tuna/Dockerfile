FROM ocaml/opam

WORKDIR /app

COPY packages dune dune-project tunac.opam tunac.opam.locked .

RUN sudo apt update -y && \
    sudo apt install -y libgmp-dev pkg-config && \
    opam install dune && \
    opam install . --deps-only

RUN eval $(opam env) && dune build --release tunac/bin/tunacc_test_operation.exe

RUN sudo install  ./_build/default/tunac/bin/tunacc_test_operation.exe /usr/bin/tunac
