{
  description = "michelson to wasm compiler";

  nixConfig = {
    extra-substituters = "https://anmonteiro.nix-cache.workers.dev";
    extra-trusted-public-keys = "ocaml.nix-cache.com-1:/xI2h2+56rwFfKyyFVbkJSeGqSIYMC/Je+7XXqGKDIY=";
  };

  inputs = {
    nixpkgs.url = "github:nix-ocaml/nix-overlays";
    flake-utils.url = "github:numtide/flake-utils";
    nix-filter.url = "github:numtide/nix-filter";

    tezos.url = "github:marigold-dev/tezos-nix";
    tezos.inputs = {
      nixpkgs.follows = "nixpkgs";
    };
  };

  outputs = {
    self,
    nixpkgs,
    flake-utils,
    nix-filter,
    tezos
  }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = (import nixpkgs {
          inherit system;
        }).appendOverlays [
            tezos.overlays.default
            (final: prev: {
              ocamlPackages = prev.ocaml-ng.ocamlPackages_4_14;
            })
        ];
      in
    {
      packages = {
        default = pkgs.ocamlPackages.callPackage ./nix/tuna.nix { inherit nix-filter; };
        vm_library = pkgs.callPackage ./nix/vm_library.nix { inherit nix-filter; };
        tuna = self.packages.${system}.default;
      };
    });
}
