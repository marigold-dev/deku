{
  description = "Nix Flake";

  nixConfig = {
    extra-substituters = "https://anmonteiro.nix-cache.workers.dev";
    extra-trusted-public-keys =
      "ocaml.nix-cache.com-1:/xI2h2+56rwFfKyyFVbkJSeGqSIYMC/Je+7XXqGKDIY=";
  };

  inputs = {
    nixpkgs.url = "github:nix-ocaml/nix-overlays";
    flake-utils.url = "github:numtide/flake-utils";
    nix-filter.url = "github:numtide/nix-filter";
    pre-commit-hooks.url = "github:cachix/pre-commit-hooks.nix";

    dream2nix.url = "github:nix-community/dream2nix";
    dream2nix.inputs.nixpkgs.follows = "nixpkgs";

    tezos.url = "github:marigold-dev/tezos-nix";
    tezos.inputs = { nixpkgs.follows = "nixpkgs"; };
    deploy-rs.url = "github:serokell/deploy-rs";

    wasm-vm.url = "github:marigold-dev/tuna";
    wasm-vm.inputs = { nixpkgs.follows = "nixpkgs"; };
    okrs-git-hook.url = "github:marigold-dev/okrs-git-hook";
    okrs-git-hook.inputs = { nixpkgs.follows = "nixpkgs"; };
  };

  outputs = { self, nixpkgs, flake-utils, nix-filter, pre-commit-hooks
    , dream2nix, tezos, deploy-rs, wasm-vm, okrs-git-hook }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = (nixpkgs.makePkgs {
          inherit system;
          extraOverlays = [
            tezos.overlays.default
            (import ./nix/overlay.nix)
            (final: prev: { ocamlPackages = prev.ocaml-ng.ocamlPackages_5_00; })
          ];
        });
        dream2nix-lib = dream2nix.lib.init {
          inherit pkgs;
          config.projectRoot = ./.;
        };
        nodejs = pkgs.nodejs-16_x;
        npmPackages = import ./nix/npm.nix {
          inherit system dream2nix-lib nix-filter nodejs;
        };

        deku = { static }:
          let
            callPackage = if static then
              pkgs.pkgsCross.musl64.callPackage
            else
              pkgs.callPackage;
          in callPackage ./nix {
            inherit nodejs npmPackages static;
            doCheck = true;
          };

        cookie-game = pkgs.callPackage ./nix/cookie-game.nix {
          inherit dream2nix-lib nix-filter nodejs;
        };

        ligo = pkgs.callPackage ./nix/ligo.nix { };
        docker = pkgs.callPackage ./nix/docker.nix {
          deku = deku { static = false; };
        };
      in rec {
        packages = let deku_static = deku { static = true; };
        in {
          inherit cookie-game docker;
          default = deku_static;
          deku = deku_static;
        };
        apps = {
          node = {
            type = "app";
            program = "${self.packages.${system}.deku}/bin/deku-node";
          };
          cli = {
            type = "app";
            program = "${deku}/bin/deku-cli";
          };
          bootstrap = {
            type = "app";
            program = "${self.packages.${system}.deku}/bin/deku-bootstrap";
          };
          benchmark = {
            type = "app";
            program = "${self.packages.${system}.deku}/bin/deku-benchmark";
          };
          generate-identity = {
            type = "app";
            program =
              "${self.packages.${system}.deku}/bin/deku-generate-identity";
          };
          cookie-game = let
            script = pkgs.writeScriptBin "cookie-game" ''
              export NODE_PATH="${cookie-game}/lib/node_modules/cookie-game/node_modules"
              ${pkgs.nodejs-16_x}/bin/node ${cookie-game}/lib/node_modules/cookie-game/lib/src/index.js "$@"
            '';
          in {
            type = "app";
            # FIXME: should we standardize the version of node used with an overlay?
            program = "${script}/bin/cookie-game";
          };
          wasm-vm = let
            script = pkgs.writeScriptBin "wasm-vm" ''
              RUST_LOG=info ${
                wasm-vm.packages."${system}".vm_library
              }/bin/vm_library "$@"
            '';
          in {
            type = "app";
            program = "${script}/bin/wasm-vm";
          };
        };
        devShells.default = import ./nix/shell.nix {
          inherit pkgs ligo;
          inherit (self.checks.${system}.pre-commit-check) shellHook;
          deku = deku { static = false; };
          deploy-rs = deploy-rs.packages.${system}.default;
        };
        checks = {
          pre-commit-check = pre-commit-hooks.lib.${system}.run {
            src = ./.;
            hooks = {
              nixfmt.enable = true;
              okr = {
                enable = true;
                name = "OKR";
                entry =
                  "${okrs-git-hook.packages.${system}.default}/bin/commit-msg";
                language = "script";
                raw.args = [ ".git/hooks/tmp" ];

                pass_filenames = false;
              };
            };
          };
        };
      }) // {
        nixosModules = {
          deku-node = import ./nix/service.nix {
            deku-packages = self.packages;
            inherit wasm-vm;
          };
        };
        deploy = {
          nodes = import ./networks/betanets/fleet.nix {
            inherit nixpkgs deploy-rs;
            deku-node = self.nixosModules.deku-node;
            rev = if self ? rev then self.rev else "dirty";
          };
          sshUser = "root";
        };
        # checks = builtins.mapAttrs (system: deployLib: deployLib.deployChecks self.deploy) deploy-rs.lib;
      };
}
