const { pnpPlugin } = require("./esbuild-plugin-esy.js");

require("esbuild")
  .build({
    plugins: process.env["NODE_PATH"] ? [] : [pnpPlugin()],
    platform: "node",
    entryPoints: [process.argv[2]],
    minify: true,
    bundle: true,
    outfile: process.argv[3],
  })
  .catch(() => process.exit(1));
