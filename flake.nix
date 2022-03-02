{
  description = "Deku development environment";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixos-unstable";
    flake-utils.url = "github:numtide/flake-utils";
    ocaml-overlays.url = "github:anmonteiro/nix-overlays";
    ocaml-overlays.inputs.nixpkgs.follows = "nixpkgs";

    esy-fhs.url = "github:d4hines/esy-fhs";
    esy-fhs.inputs.nixpkgs.follows = "nixpkgs";
    esy-fhs.inputs.anmonteiro.follows = "ocaml-overlays";
    esy-fhs.inputs.flake-utils.follows = "flake-utils";
  };

  outputs = { self, nixpkgs, flake-utils, ocaml-overlays, esy-fhs }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = import nixpkgs {
          inherit system;
          overlays = [ ocaml-overlays.overlay ];
        };

        esy = esy-fhs.packages.${system}.esy;
        devShell = import ./nix/shell.nix { inherit pkgs; inherit esy; };
      in
      {
        inherit esy devShell;
      });
}
