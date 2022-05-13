{ system, dream2nix-lib, nix-filter, nodejs }:

let
  outputs = (dream2nix-lib.makeFlakeOutputs {
    source = nix-filter.lib.filter {
      root = ../.;
      include = [ ../package-lock.json ../package.json ];
    };

    packageOverrides = {
      webpack-cli = {
        remove-webpack-check = {
          patches = [ ./patches/remove-webpack-check.patch ];
        };
      };
    };

    # TODO: If we remove this Webpack will fail, not sure why
    inject = { acorn-import-assertions."1.8.0" = [[ "acorn" "8.7.1" ]]; };

    settings =
      [{ subsystemInfo.nodejs = (builtins.substring 0 2 nodejs.version); }];
  });
  npmPackages = outputs.packages."${system}".sidechain;

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

in node_modules
