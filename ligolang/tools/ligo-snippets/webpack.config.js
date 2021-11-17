const path = require('path');
const nodeExternals = require("webpack-node-externals");

module.exports = {
    mode: 'production',
    entry: './src/index.tsx',
    output: {
        path: path.resolve('lib'),
        filename: 'index.js',
        libraryTarget: 'commonjs2'
    },
    module: {
        rules: [{
                test: /\.(t|j)sx?$/,
                exclude: /(node_modules)/,
                use: 'babel-loader'
            },
            {
                test: /\.css/,
                use: ["style-loader", "css-loader"],
            },
        ]
    },
    externals: [nodeExternals()],
    resolve: {
        extensions: ['.ts', '.tsx', '.js', '.jsx']
    },
}