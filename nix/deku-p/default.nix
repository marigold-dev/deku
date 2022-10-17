{ self, inputs, ...}:

{
  flake = {
    nixosModules = {
      deku-node = import ./service.nix { deku-packages = self.packages; };
    };
  };

  perSystem = {
    config, self', inputs', system, pkgs, dream2nix-lib, nodejs,...
  }: let
      npmPackages = import ./npm.nix {
        inherit system dream2nix-lib nodejs;
        inherit (inputs) nix-filter;
      };

      typescript-sdk = pkgs.callPackage ./typescript-sdk.nix {
        inherit nodejs dream2nix-lib;
        inherit (inputs) nix-filter;
      };

      deku = pkgs.callPackage ./deku.nix {
        inherit nodejs npmPackages;
        inherit (inputs) nix-filter;
        doCheck = true;
      };

      ligo = inputs.ligo.packages.${system}.ligoLight;
    in

    {
      packages = { default = deku; inherit deku typescript-sdk; };
      apps = {
        node = {
          type = "app";
          program = "${deku}/bin/deku-node";
        };
        bootstrap = {
          type = "app";
          program = "${deku}/bin/deku-bootstrap";
        };
        benchmark = {
          type = "app";
          program = "${deku}/bin/deku-benchmark";
        };
        generate-identity = {
          type = "app";
          program = "${deku}/bin/deku-generate-identity";
        };
      };
  };
}