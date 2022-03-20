module.exports = (env) => ({
  entry: {
    tezos_js_bridge: "./tezos_js_bridge.js",
    listen_transactions: "./listen_transactions.js",
  },
  output: {
    filename: "[name].bundle.js",
    path: __dirname,
  },
  mode: env.dev ? "development" : "production",
  target: "node",
});
