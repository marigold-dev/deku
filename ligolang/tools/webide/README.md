# Quick Start

1. Install `yarn`
1. Run `yarn` to install dependencies
1. Install Ligo compiler by following [Ligo installation instructions][install-ligo]
1. Go to `packages/server/` directory
1. Run `yarn start` to start the development server
1. Open http://localhost:8080

# Dependency on Examples

Examples that are displayed in the Web IDE are curated from `/src/test/examples` folder and packaged during the build of the client. To add a new example to the Web IDE, first add the example file to `/src/test/examples` folder; it may live under any level of subdirectories. Then, add the path to the example to the `CURATED_EXAMPLES` array in the `packages/client/package-examples.js` script. The path has to be relative to `/src/test/examples`.

## Server

The server source code is located under `packages/server/`. Please see README under `packages/server/` for information about how to get started on the server development.

You may use the static binary of Ligo by setting the LIGO_CMD environment variable. Otherwise this project assumes that docker is available, and will use the Ligo docker image to run Ligo.

[install-ligo]: https://ligolang.org/docs/intro/installation/

## Client

The client source code is located under `packages/client/`.  See the README under the `packages/client/` for information about how to get started on the client development.
