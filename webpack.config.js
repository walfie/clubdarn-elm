var path = require('path');

var CleanWebpackPlugin = require('clean-webpack-plugin');
var CopyWebpackPlugin = require('copy-webpack-plugin');
var ExtractTextPlugin = require("extract-text-webpack-plugin");
var HtmlWebpackPlugin = require('html-webpack-plugin');
var webpack = require('webpack');

var paths = {
  dist: path.resolve('./dist'),
  entry: path.resolve('./src/js/index.js'),
  template: path.resolve('./src/index.html'),
  elmMake: path.resolve('./node_modules/.bin/elm-make')
}

module.exports = {
  entry: paths.entry,
  module: {
    noParse: /\.elm$/,
    rules: [
      {
        test: /\.css$/,
        use: ExtractTextPlugin.extract({
          use: 'css-loader?minimize=true'
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
    filename: 'assets/[name].[chunkhash:8].js',
    path: paths.dist
  },
  plugins: [
    new ExtractTextPlugin("assets/[name].[chunkhash:8].css"),
    new HtmlWebpackPlugin({
      inject: true,
      template: paths.template
    }),
    new CopyWebpackPlugin([
      { from: 'src/static', to: path.resolve(paths.dist, 'assets') }
    ]),
    new webpack.optimize.UglifyJsPlugin({}),
    new CleanWebpackPlugin(['dist'], {
      root: __dirname,
      verbose: true,
      dry: false
    })
  ]
};

