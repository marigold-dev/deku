{
  description = "Nix Flake";

  nixConfig = {
    extra-substituters = "https://anmonteiro.nix-cache.workers.dev";
    extra-trusted-public-keys = "ocaml.nix-cache.com-1:/xI2h2+56rwFfKyyFVbkJSeGqSIYMC/Je+7XXqGKDIY=";
  };

  inputs = {
    nixpkgs.url = "github:nix-ocaml/nix-overlays";
    nix-filter.url = "github:numtide/nix-filter";
    flake-parts.url = "github:hercules-ci/flake-parts";
    flake-parts.inputs.nixpkgs.follows = "nixpkgs";

    dream2nix.url = "github:nix-community/dream2nix";
    dream2nix.inputs.nixpkgs.follows = "nixpkgs";

    ligo.url = "gitlab:ligolang/ligo";
    tezos.url = "github:marigold-dev/tezos-nix";
    tezos.inputs = {
      nixpkgs.follows = "nixpkgs";
      flake-parts.follows = "flake-parts";
    };
    rust-overlay.url = github:oxalica/rust-overlay;
    rust-overlay.inputs = {
      nixpkgs.follows = "nixpkgs";
    };
    alien_ffi.url = "github:d4hines/alien_ffi";
    alien_ffi.inputs.rust-overlay.follows = "rust-overlay";
    alien_ffi.inputs.nixpkgs.follows = "nixpkgs";
    alien_ffi.inputs.flake-utils.follows = "rust-overlay/flake-utils";

    ocaml-dalek = {
      url = "github:d4hines/ocaml-dalek";
      inputs.nixpkgs.follows = "nixpkgs";
      inputs.alien_ffi.follows = "alien_ffi";
    };

    deploy-rs.url = "github:serokell/deploy-rs";
  };

  outputs = { self, nixpkgs, flake-parts, nix-filter, dream2nix, tezos, deploy-rs, ligo, ocaml-dalek, ... }:
    flake-parts.lib.mkFlake { inherit self; } {
      imports = [
        ./nix/js-packages.nix
        ./nix/deku-c
        ./nix/deku-p
      ];
      systems = [
        "x86_64-linux"
        "aarch64-darwin"
        "x86_64-darwin"
      ];
      perSystem = { config, system, self', pkgs, ... }:
        {
          config = {
            _module.args = {
              pkgs = (nixpkgs.makePkgs {
                inherit system;
                extraOverlays = [
                  tezos.overlays.default
                  (import ./nix/overlay.nix)
                  (final: prev:
                    let
                      libdalek_rs-pkg = ocaml-dalek.packages."${system}".libdalek_rs;
                      ocaml-dalek-pkg = ocaml-dalek.packages."${system}".ocaml-dalek prev.ocaml-ng.ocamlPackages_5_00;
                      all = prev.symlinkJoin {
                        name = "all";
                        paths = [ ocaml-dalek-pkg libdalek_rs-pkg ];
                      };
                    in
                    {
                      ocamlPackages = prev.ocaml-ng.ocamlPackages_5_00 // { ocaml-dalek = all; };
                    })
                ];
              });
              nodejs = pkgs.nodejs-16_x;
              dream2nix-lib = dream2nix.lib.init {
                inherit pkgs;
                config.projectRoot = ./.;
              };
            };
            devShells.default = import ./nix/shell.nix {
              inherit pkgs;
              ligo = if system == "x86_64-linux" then ligo.packages.${system}.ligoLight else pkgs.hello;
              deku = self'.packages.deku;
              tuna = self'.packages.tuna;
              vm_library = self'.packages.vm_library;
              deploy-rs = deploy-rs.packages.${system}.default;
            };
          };
        };
      flake = {
        deploy = {
          nodes = import ./networks/betanets/fleet.nix {
            inherit nixpkgs deploy-rs;
            deku-node = self.nixosModules.deku-node;
            rev = if self ? rev then self.rev else "dirty";
          };
          sshUser = "root";
        };

        checks = builtins.mapAttrs (system: deployLib: deployLib.deployChecks self.deploy) deploy-rs.lib;
      };
    };
}
