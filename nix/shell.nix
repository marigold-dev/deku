{
  pkgs,
  deku,
  ligo,
  tuna,
  deploy-rs,
}:
with pkgs;
with ocamlPackages; let
  rust-src = stdenv.mkDerivation {
    inherit (rustc) src;
    inherit (rustc.src) name;
    phases = ["unpackPhase" "installPhase"];
    installPhase = ''cp -r library $out'';
  };
in
  mkShell {
    inputsFrom = [deku tuna];
    shellHook = ''
      export RUST_SRC_PATH="${rust-src}"
    '';
    packages = [
      # Rust gameboy deps
      pkg-config
      cargo
      rustc
      libiconv
      xorg.libX11
      xorg.libXcursor
      xorg.libXrandr
      xorg.libXi
      xorg.libXext
      libGLU
      alsa-lib

      # Formatters
      alejandra
      ocamlformat
      nodePackages.prettier
      rustfmt

      rust-analyzer

      # Typescript for decookie
      nodePackages.typescript

      # Tezos tooling
      ligo

      # OCaml developer tooling
      ocaml
      dune_3
      ocaml-lsp
      ocamlformat-rpc
      utop

      # deployment
      deploy-rs

      # helpful tooling
      bc
      sqlite
      termdbms
    ];
  }
