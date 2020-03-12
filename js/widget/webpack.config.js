var path = require('path');

module.exports = {
    entry: "./index.js",
    output: {
        filename: 'index.built.js',
        path: path.resolve(__dirname, 'built'),
        publicPath: 'built/'
    },
    resolve: {
        alias: {
            '@jupyterlab/services/kernel/future': path.resolve(__dirname, 'node_modules/@jupyterlab/services/lib/kernel/future'),
            '@jupyterlab/services/kernel/comm': path.resolve(__dirname, 'node_modules/@jupyterlab/services/lib/kernel/comm')
        }
    },
    module: {
        rules: [
            { test: /\.css$/, loader: "style-loader!css-loader" },
            // jquery-ui loads some images
            { test: /\.(jpg|png|gif)$/, use: 'file-loader' },
            // required to load font-awesome
            { test: /\.woff2(\?v=\d+\.\d+\.\d+)?$/, use: 'url-loader?mimetype=application/font-woff' },
            { test: /\.woff(\?v=\d+\.\d+\.\d+)?$/, use: 'url-loader?mimetype=application/font-woff' },
            { test: /\.ttf(\?v=\d+\.\d+\.\d+)?$/, use: 'url-loader?mimetype=application/octet-stream' },
            { test: /\.eot(\?v=\d+\.\d+\.\d+)?$/, use: 'file-loader' },
            { test: /\.svg(\?v=\d+\.\d+\.\d+)?$/, use: 'url-loader?mimetype=image/svg+xml' }
        ]
    },
}
