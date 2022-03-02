{ pkgs, esy ? pkgs.nodePackages.esy, deku, npmPackages }:

pkgs.mkShell {
  shellHook = deku.configurePhase;

  nativeBuildInputs = (with pkgs; [
    # Make developer life easier
    ## General tooling
    docker
    docker-compose
    nodejs-12_x
    ligo

    # Nix files formatter
    nixfmt
  ]) ++ (with pkgs.ocaml-ng.ocamlPackages_5_00; [
    # OCaml developer tooling
    ocaml
    findlib
    dune_2
    # esy
    ocaml-lsp
    ocamlformat-rpc
  ]) ++ deku.nativeBuildInputs;

  propagatedBuildInputs = deku.buildInputs;
}