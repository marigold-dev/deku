{ dream2nix-lib, nix-filter, nodejs }:

let
  outputs = (dream2nix-lib.makeOutputs {
    source = nix-filter.lib.filter {
      root = ../.;
      include = [ "./examples/cookie-game" "./sdks/typescript-sdk" ];
    };

    settings =
      [{ subsystemInfo.nodejs = (builtins.substring 0 2 nodejs.version); }];
  });
in

outputs.packages.cookie-game