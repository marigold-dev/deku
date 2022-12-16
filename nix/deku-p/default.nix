{
  self,
  inputs,
  ...
}: {
  flake = {
    nixosModules = {
      deku-node = import ./service.nix {deku-packages = self.packages;};
    };
  };

  perSystem = {
    config,
    self',
    inputs',
    system,
    pkgs,
    dream2nix-lib,
    nodejs,
    ...
  }: let
    pkgs' = pkgs.pkgsCross.musl64;

    npmPackages = import ./npm.nix {
      inherit system dream2nix-lib nodejs;
      inherit (inputs) nix-filter;
    };

    deku = pkgs.callPackage ./deku.nix {
      inherit nodejs npmPackages;
      inherit (inputs) nix-filter;
      doCheck = true;
    };

    deku-static = pkgs'.callPackage ./deku.nix {
      inherit nodejs npmPackages;
      inherit (inputs) nix-filter;
      doCheck = true;
      static = true;
    };

    ligo =
      # TODO: support ligo on more systems in our flake
      if system == "x86_64-linux"
      then inputs.ligo.packages.${system}.ligoLight
      else pkgs.hello;

    docker = pkgs.callPackage ./docker.nix {inherit deku ligo;};
  in {
    packages = {
      default = deku;
      inherit deku deku-static docker;
    };
    apps = {
      node = {
        type = "app";
        program = "${deku}/bin/deku-node";
      };
      benchmark = {
        type = "app";
        program = "${deku}/bin/deku-benchmark";
      };
      generate-identity = {
        type = "app";
        program = "${deku}/bin/deku-generate-identity";
      };
      ligo-deku-rpc = {
        type = "app";
        program = "${deku}/bin/ligo-deku-rpc";
      };
    };
  };
}
