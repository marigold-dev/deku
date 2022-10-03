{ system, dream2nix-lib, nix-filter, nodejs }:

let
  outputs = (dream2nix-lib.makeOutputs {
    source = nix-filter.lib.filter {
      root = ../.;
      include = [ ../deku-p/src/tezos_interop/package-lock.json ../deku-p/src/tezos_inteorp/package.json ];
    };

    packageOverrides = {
      webpack-cli = {
        remove-webpack-check = {
          patches = [ ./patches/remove-webpack-check.patch ];
        };
      };
    };

    settings =
      [{ subsystemInfo.nodejs = (builtins.substring 0 2 nodejs.version); }];
  });
  npmPackages = outputs.packages.sidechain;

  node_modules' = builtins.attrValues
    (builtins.removeAttrs npmPackages.dependencies [ "webpack" ]);
  webpack = npmPackages.dependencies.webpack.overrideAttrs (_: {
    postFixup = ''
      wrapProgram $out/bin/webpack \
      --set NODE_PATH=${npmPackages}/lib/node_modules/sidechain/node_modules:${
        builtins.foldl' (prev: curr: prev + ":" + curr) "$NODE_PATH"
        node_modules'
      }
    '';
  });
  node_modules = node_modules' ++ [ webpack ];

in
node_modules
