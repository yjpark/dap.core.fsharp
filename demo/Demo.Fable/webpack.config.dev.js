var path = require("path");
var webpack = require("webpack");
var common = require("./webpack.config.common");

console.log("Bundling for development...");

buildDir = common.resolve("build"),

module.exports = {
  devtool: "source-map",
  entry: common.config.entry,
  output: {
    filename: '[name].js',
    path: buildDir,
    devtoolModuleFilenameTemplate: info =>
      path.resolve(info.absoluteResourcePath).replace(/\\/g, '/'),
  },
  devServer: {
    contentBase: common.config.publicDir,
    publicPath: '/',
    host: '0.0.0.0',
    allowedHosts: [
      'localhost',
      'pettyfun.com'
    ],
    port: 8080,
    hot: true,
    inline: true,
    proxy: {
      '/ws_user': {
        target1: "http://localhost:5600",
        target: "http://pettyfun.com:5600",
        changeOrigin: true,
        ws: true
      }
    }
  },
  module: {
    rules: common.getModuleRules()
  },
  plugins: common.getPlugins(buildDir).concat([
      new webpack.HotModuleReplacementPlugin(),
      new webpack.NamedModulesPlugin()
  ]),
  resolve: {
    modules: [common.config.nodeModulesDir]
  },
};
