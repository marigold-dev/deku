module.exports = env => ({
  entry: ['./run_entrypoint.js'],
  output: {
    filename: 'run_entrypoint.bundle.js',
    path: __dirname,
  },
  mode: env.dev ? 'development' : 'production',
  target: 'node'
});
