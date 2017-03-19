var path = require('path');

var CleanWebpackPlugin = require('clean-webpack-plugin');
var CopyWebpackPlugin = require('copy-webpack-plugin');
var ExtractTextWebpackPlugin = require("extract-text-webpack-plugin");
var HtmlWebpackPlugin = require('html-webpack-plugin');
var webpack = require('webpack');

var isProd = process.env.NODE_ENV === 'production';

var paths = {
  dist: path.resolve('./dist'),
  entry: path.resolve('./src/js/index.js'),
  template: path.resolve('./src/index.html'),
  elmMake: path.resolve('./node_modules/.bin/elm-make')
}

var nameTemplate = 'assets/[name]' + (isProd ? '.[chunkhash:8]' : '');

var plugins = [
  new ExtractTextWebpackPlugin(nameTemplate + '.css'),
  new HtmlWebpackPlugin({
    inject: false,
    template: paths.template,
    minify: {
      minifyJS: isProd
    }
  }),
  new CopyWebpackPlugin([
    { from: 'src/static', to: path.resolve(paths.dist, 'assets') }
  ])
].concat(isProd ? [
  new webpack.optimize.UglifyJsPlugin({}),
  new CleanWebpackPlugin(['dist'], {
    root: __dirname,
    verbose: true,
    dry: false
  })
]: []);

module.exports = {
  entry: paths.entry,
  module: {
    noParse: /\.elm$/,
    rules: [
      {
        test: /\.css$/,
        use: ExtractTextWebpackPlugin.extract({
          use: 'css-loader?minimize=' + isProd
        })
      },
      {
        test: /\.elm$/,
        exclude: [/elm-stuff/, /node_modules/],
        loader: 'elm-webpack-loader',
        query: {
          pathToMake: paths.elmMake,
          verbose: true,
          warn: true
        }
      }
    ]
  },
  output: {
    filename: nameTemplate + '.js',
    path: paths.dist
  },
  plugins: plugins,
  stats: isProd ? 'normal' : {
    assets: false,
    children: false,
    chunks: false,
    chunkModules: false,
    chunkOrigins: false
  }
};

