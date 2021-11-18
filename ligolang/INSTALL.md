# How to build Ligo from source

These are instructions on how to build Ligo from source in Ubuntu. For
ordinary user installs, see the [doc
website](https://ligolang.org/docs/intro/installation/).

In order to build Ligo, you may need to install:

- `opam` - OCaml package manager,
- `pkg-config` tool for finding library compilation flags,
- `cargo` - rust package manager for Rust interoperability (needed for
  the Tezos Edo protocol)
- libraries with include files:
  - `libev` - event handling library
  - `libhidapi`
  - `libffi` - foreign function interface library
  - `libgmp` - arbitrary size integer library

## Ubuntu


1. Please first install necessary build tools and libraries with:

   ```sh
   apt update
   apt install -y opam cargo make pkg-config libhidapi-dev libev-dev libgmp-dev libffi-dev
   ```

   `opam` and `cargo` can be installed via their installation scripts
   instead of from `apt`, if desired. See the
   [opam](https://opam.ocaml.org/doc/Install.html) and
   [rustup](https://www.rust-lang.org/tools/install) install
   instructions.

2. Then you may want to initialize `opam`:

   ```sh
   opam init --bare --auto-setup
   ```

3. Now you are ready to start building Ligo itself. To build and run
   the tests:

   ```sh
   make
   ```

   To only build:

   ```sh
   make build
   ```

For an example Ubuntu-based Docker image, see [`Dockerfile.ubuntu`][./Dockerfile.ubuntu].
