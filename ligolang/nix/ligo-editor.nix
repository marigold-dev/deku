{ stdenv, lib, mkYarnPackage, nodejs, python2, ligo-bin, coreutils
, writeShellScriptBin, makeFontsConf, buildEnv, rsync, sources
, chromium ? null }:
let
  # Use a common yarn.lock for everything
  yarnLock = ../tools/webide/yarn.lock;

  installPhase = "mkdir $out; cp -Lr node_modules $out/node_modules";

  # node_modules of the server
  server = mkYarnPackage {
    name = "webide-server";
    src = ../tools/webide/packages/server;
    buildPhase = ''
      cp ${../tools/webide/tsconfig.json} tsconfig.json
      yarn --offline run build
      rm node_modules/server/server
    '';
    doCheck = true;
    checkPhase = "DATA_DIR=/tmp LIGO_CMD=${../ligo} yarn --offline jest";
    distPhase = "true";
    inherit yarnLock installPhase;
  };

  # node_modules of the client
  client = mkYarnPackage rec {
    name = "webide-client";
    src = ../tools/webide/packages/client;
    buildPhase = ''
      export EXAMPLES_DIR=${../src/test/examples}
      yarn --offline run build
      rm node_modules/client/client
      find deps/client/build -type f -exec sed -r "s,/nix/store/[a-z0-9]{32}-[^/]*,$out,g" -i '{}' \;
    '';
    distPhase = "true";
    installPhase = "mkdir $out; cp -Lr deps/client/build $out";
    inherit yarnLock;
    # Downloads node-sass from the official github repo
    # Uncomment the commented lines if you wish to build it from source
    yarnPreBuild = "export SASS_BINARY_PATH=${sources.node-sass-bin}";
    /* ''
         mkdir -p "$HOME/.node-gyp/${nodejs.version}"
         echo 9 > "$HOME/.node-gyp/${nodejs.version}/installVersion"
         ln -sfv "${nodejs}/include" "$HOME/.node-gyp/${nodejs.version}"
       '';
    */
  };

  # Perform the e2e tests; output is empty on purpose
  e2e = mkYarnPackage rec {
    name = "webide-e2e";
    src = ../tools/webide/packages/e2e;
    # Provide puppeteer with chromium, since it can't download it inside the nix sandbox.
    # Also, since we override nodejs in our overlays, import chromium from pure nixpkgs to avoid a rebuild
    buildPhase = ''
      export HOME="$(pwd)"
      export PUPPETEER_SKIP_CHROMIUM_DOWNLOAD=1
      export PUPPETEER_EXECUTABLE_PATH=${(import (import ./sources.nix).nixpkgs {}).chromium.outPath}/bin/chromium
      ${ligo-editor}/bin/ligo-editor &
      export API_HOST=http://localhost:8080
      export FONTCONFIG_FILE=${makeFontsConf { fontDirectories = [ ]; }}
      yarn --offline jest
    '';
    distPhase = "true";
    installPhase = "touch $out";
    inherit yarnLock;
  };

  # Run the WebIDE server with all the needed env variables
  ligo-editor = writeShellScriptBin "ligo-editor" ''
    set -e
    LIGO_CMD=${../ligo} \
    STATIC_ASSETS=${client} \
    DATA_DIR=/tmp \
    ${nodejs}/bin/node ${server}/node_modules/server/dist/src/index.js
  '';
in ligo-editor // { inherit e2e; }
