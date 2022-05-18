{
  description = "Deku development environment";

  # Setup trusted binary caches
  nixConfig = {
    trusted-substituters =
      [ "https://cache.nixos.org/" "https://deku.cachix.org" ];
    trusted-public-keys = [
      "cache.nixos.org-1:6NCHdD59X431o0gWypbMrAURkbJ16ZPMQFGspcDShjY="
      "deku.cachix.org-1:9zYY9BUBDV9xiXj8C0uCy+hkgjyDVRdH932KHmqVwYg="
    ];
  };

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixos-unstable";
    flake-utils.url = "github:numtide/flake-utils";
    nix-filter.url = "github:numtide/nix-filter";

    dream2nix.url = "github:nix-community/dream2nix";
    dream2nix.inputs.nixpkgs.follows = "nixpkgs";

    ocaml-overlays.url = "github:anmonteiro/nix-overlays";
    ocaml-overlays.inputs = {
      nixpkgs.follows = "nixpkgs";
      flake-utils.follows = "flake-utils";
    };

    tezos.url = "github:marigold-dev/tezos-nix";
    tezos.inputs = {
      nixpkgs.follows = "nixpkgs";
      flake-utils.follows = "flake-utils";
    };

    prometheus-web.url = "github:marigold-dev/prometheus-web";
    prometheus-web.inputs = {
      nixpkgs.follows = "nixpkgs";
      ocaml-overlay.follows = "ocaml-overlays";
      flake-utils.follows = "flake-utils";
    };

    pollinate.url = "github:marigold-dev/pollinate/chore/stateIsUseless";
    pollinate.inputs = {
      nixpkgs.follows = "nixpkgs";
      ocaml-overlay.follows = "ocaml-overlays";
      flake-utils.follows = "flake-utils";
    };
  };

  outputs = { self, nixpkgs, flake-utils, nix-filter, dream2nix, ocaml-overlays
    , prometheus-web, tezos, pollinate }:
    let
      supportedSystems =
        [ "x86_64-linux" "x86_64-darwin" "aarch64-linux" "aarch64-darwin" ];

      dream2nix-lib = dream2nix.lib2.init {
        systems = supportedSystems;
        config.projectRoot = ./.;
      };
    in with flake-utils.lib;
    eachSystem supportedSystems (system:
      let
        nodejs = pkgs.nodejs-16_x;

        ligo = (import nixpkgs { inherit system; }).ligo.overrideAttrs
          (_: { meta = { platforms = pkgs.ocaml.meta.platforms; }; });

        pkgs = ocaml-overlays.makePkgs {
          inherit system;
          extraOverlays = [
            (import ./nix/overlay.nix)
            prometheus-web.overlays.default
            tezos.overlays.default
            pollinate.overlays.default
          ];
        };

        pkgs_static = pkgs.pkgsCross.musl64;
        bp =
          pkgs.callPackage nix-npm-buildpackage { nodejs = pkgs.nodejs-14_x; };

        npmPackages = import ./nix/npm.nix {
          inherit system dream2nix-lib nix-filter nodejs;
        };

        deku = pkgs.callPackage ./nix/deku.nix {
          doCheck = true;
          inherit nodejs npmPackages;
        };

        deku-static = pkgs_static.callPackage ./nix/deku.nix {
          pkgs = pkgs_static;
          doCheck = true;
          static = true;
          inherit nodejs npmPackages;
        };

        sandbox = pkgs.callPackage ./nix/sandbox.nix {
          inherit deku ligo;
          pkgs = import nixpkgs { inherit system; };
        };
      in {
        devShell = import ./nix/shell.nix { inherit pkgs deku ligo; };
        packages = {
          inherit deku deku-static;
          docker = import ./nix/docker.nix {
            inherit pkgs;
            deku = deku-static;
          };
        }
        # Add npm packages as exposed packages
          // (builtins.listToAttrs (builtins.map (p: {
            name = p.pname;
            value = p;
          }) npmPackages));

        apps = {
          deku-cli = {
            type = "app";
            program = "${deku}/bin/deku-cli";
          };
          deku-node = {
            type = "app";
            program = "${deku}/bin/deku-node";
          };
          sandbox = {
            type = "app";
            program = "${sandbox}/bin/sandbox.sh";
          };
        };
      }) // {
        hydraJobs = {
          x86_64-linux = self.packages.x86_64-linux;
          # darwin doesn't support static builds and docker
          aarch64-darwin = builtins.removeAttrs self.packages.aarch64-darwin [
            "deku-static"
            "docker"
          ];
        };
      };
}
