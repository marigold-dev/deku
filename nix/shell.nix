{ pkgs, system, deku, ligo, nodejs ? pkgs.nodejs }:
pkgs.mkShell {
  shellHook = ''
    export PATH="$(pwd)/_build/install/default/bin:$PATH"

    # Triggers a rebuild before executing the rest of the script.
    export REBUILD=y
  '';
  inputsFrom = [ deku ];
  packages = with pkgs;
    [
      # Make developer life easier
      ## General tooling
      docker
      nodejs
      shellcheck

      docker-compose # This is needed by tilt
      jq

      # Tezos tooling
      ligo

      # formatters
      nixfmt
      nodePackages.prettier
    ] ++ (pkgs.lib.optional (system != "x86_64-darwin") tilt)
    ++ (with pkgs.ocaml-ng.ocamlPackages_5_00; [
      # OCaml developer tooling
      ocaml
      findlib
      dune_2
      odoc
      ocaml-lsp
      ocamlformat-rpc_0_20_1 # 0.21 is incompatible with latest cmdliner
    ]);
}
