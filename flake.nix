{
  description = "Deku development environment";

  # Setup trusted binary caches
  nixConfig = {
    extra-substituters = [ "https://deku.cachix.org" ];
    extra-public-keys =
      [ "deku.cachix.org-1:9zYY9BUBDV9xiXj8C0uCy+hkgjyDVRdH932KHmqVwYg=" ];
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

    json-logs-reporter.url = "github:marigold-dev/json-logs-reporter";
    json-logs-reporter.inputs = {
      nixpkgs.follows = "nixpkgs";
      nix-filter.follows = "nix-filter";
      flake-utils.follows = "flake-utils";
    };
  };

  outputs = { self, nixpkgs, flake-utils, nix-filter, dream2nix, ocaml-overlays
    , prometheus-web, tezos, json-logs-reporter }:
    let supportedSystems = [ "x86_64-linux" "aarch64-linux" "aarch64-darwin" ];
    in with flake-utils.lib;
    eachSystem supportedSystems (system:
      let
        pkgs = (ocaml-overlays.makePkgs { inherit system; }).appendOverlays [
          (import ./nix/overlay.nix)
          prometheus-web.overlays.default
          tezos.overlays.default
          json-logs-reporter.overlays.default
          (self: super: {
            ocamlPackages = super.ocaml-ng.ocamlPackages_5_00;
          })
        ];

        pkgs_static = pkgs.pkgsCross.musl64;

        dream2nix-lib = dream2nix.lib.init {
          inherit pkgs;
          config.projectRoot = ./.;
        };
        nodejs = pkgs.nodejs-16_x;

        ligo = (import nixpkgs { inherit system; }).ligo.overrideAttrs
          (_: { meta = { platforms = pkgs.ocaml.meta.platforms; }; });

        npmPackages = import ./nix/npm.nix {
          inherit system dream2nix-lib nix-filter nodejs;
        };

        deku = pkgs.callPackage ./nix/deku.nix {
          doCheck = true;
          inherit nix-filter nodejs npmPackages;
        };

        deku-static = pkgs_static.callPackage ./nix/deku.nix {
          pkgs = pkgs_static;
          doCheck = true;
          static = true;
          inherit nix-filter nodejs npmPackages;
        };

        sandbox = pkgs.callPackage ./nix/sandbox.nix {
          inherit deku ligo;
          pkgs = import nixpkgs { inherit system; };
        };
      in {
        devShell = import ./nix/shell.nix { inherit pkgs system deku ligo; };
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

        formatter = pkgs.callPackage ./nix/formatter.nix { };
      }) // {
        hydraJobs = {
          x86_64-linux = self.packages.x86_64-linux;
          aarch64-darwin = {
            # darwin doesn't support static builds and docker
            inherit (self.packages.aarch64-darwin) deku npmPackages;
          };
        };
      };
}
