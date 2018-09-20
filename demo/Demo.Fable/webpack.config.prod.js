var webpack = require("webpack");
var common = require("./webpack.config.common");
var CopyWebpackPlugin = require('copy-webpack-plugin');
var path = require("path");

console.log("Bundling for production...");

buildDir = common.resolve("../Fable.Web/wwwroot"),

module.exports = {
  entry: common.config.entry,
  output: {
    filename: '[name].[hash].js',
    path: buildDir
  },
  module: {
    rules: common.getModuleRules()
  },
  plugins: common.getPlugins(buildDir, true).concat([
    // new ExtractTextPlugin('style.css'),
    new CopyWebpackPlugin([ { from: common.config.publicDir } ])
  ]),
  resolve: {
    modules: [common.config.nodeModulesDir]
  },
};
