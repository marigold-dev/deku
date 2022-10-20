# This file is specifically for the Deku Tezos bridge
# It's only used internally in the Deku node and not
# part of our client-facing code. Hence it's separate
# from the npm workspace in ../../package.json
{ system, dream2nix-lib, nix-filter, nodejs }:

let
  outputs = (dream2nix-lib.makeOutputs {
    source = nix-filter.lib.filter {
      root = ../../deku-p/src/tezos_interop;
      include = [ "package-lock.json" "package.json" "tezos_js_bridge.js" "webpack.config.json" ];
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
