module.exports = env => ({
  entry: {
    run_entrypoint: './run_entrypoint.js',
    listen_transactions: "./listen_transactions.js"
  },
  output: {
    filename: '[name].bundle.js',
    path: __dirname,
  },
  mode: env.dev ? 'development' : 'production',
  target: 'node'
});
