{
  description = "Deku development environment";
  inputs.nixpkgs.url = "github:nixos/nixpkgs/nixos-unstable";
  inputs.flake-utils.url = "github:numtide/flake-utils";
  inputs.ocaml-overlays.url = "github:anmonteiro/nix-overlays/ulrikstrid/deku-exploration";
  inputs.ocaml-overlays.inputs.nixpkgs.follows = "nixpkgs";

  outputs = { self, nixpkgs, flake-utils, ocaml-overlays }:
    flake-utils.lib.eachDefaultSystem (system:
      let pkgs = import nixpkgs { inherit system; overlays = [ocaml-overlays.overlay]; };
      in
      {
        devShell = (pkgs.mkShell {
          shellHook = ''
            npm install
            export PATH="node_modules/.bin:_build/default/bin:$PATH"
          '';

          packages = with pkgs.ocaml-ng.ocamlPackages_multicore; [
            ocaml
            dune_2
            findlib
            ocaml-lsp
            ocamlformat-rpc
            pkgs.nodejs
            pkgs.nodePackages.npm
          ];

          propagatedBuildInputs = with pkgs.ocaml-ng.ocamlPackages_multicore; [
            reason
            ppx_deriving
            ppx_deriving_yojson
            lwt
            mirage-crypto
            mirage-crypto-pk
            mirage-crypto-rng
            mirage-crypto-ec
            opium
            piaf
            mrmime
            hex
            tezos-micheline
            digestif
            cmdliner
            ppx_blob
            secp256k1-internal
            bigstring
            domainslib
            reason-native.rely
            utop
            rtop
          ];
        });
      }
    );
}
