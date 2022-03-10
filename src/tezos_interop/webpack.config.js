// In Nix environments, we want to use
// a custom NODE_PATH defined in shell.nix
// Otherwise, we want to omit this config entirely.
const resolve = process.env.NODE_PATH
  ? {
      resolve: {
        modules: [process.env.NODE_PATH],
      },
    }
  : {};

module.exports = (env) => ({
  ...resolve,
  entry: {
    run_entrypoint: "./run_entrypoint.js",
    fetch_storage: "./fetch_storage.js",
    listen_transactions: "./listen_transactions.js",
  },
  output: {
    filename: "[name].bundle.js",
    path: __dirname,
  },
  mode: env.dev ? "development" : "production",
  target: "node",
});
