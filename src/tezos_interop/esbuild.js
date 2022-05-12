require("esbuild")
  .build({
    platform: "node",
    plugins: process.env["NODE_PATH"]
      ? []
      : [require("./esbuild-plugin-esy.js").pnpPlugin()],
    entryPoints: [process.argv[2]],
    minify: true,
    bundle: true,
    outfile: process.argv[3],
  })
  .catch(() => process.exit(1));
