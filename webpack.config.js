const path = require("path");
const ExtractTextPlugin = require("extract-text-webpack-plugin");
const CleanWebpackPlugin = require('clean-webpack-plugin')


const extractSass = new ExtractTextPlugin({
  filename: "app.css",
  disable: process.env.NODE_ENV === "development"
});

// var elmSource = path.resolve(__dirname + '/src');

module.exports = {
  entry: './src/index.js',

  output: {
    path: path.resolve(__dirname + '/dist'),
    filename: 'app.js',
  },

  module: {
    rules: [
      {
        test: /\.scss$/,
        use: extractSass.extract({
            use: ["css-loader", "sass-loader"],
            // use style-loader in development
            fallback: "style-loader"
        })
      },
      
      {
        test:    /\.html$/,
        exclude: /node_modules/,
        loader:  'file-loader?name=[name].[ext]',
      },
      {
        test:    /\.elm$/,
        exclude: [/elm-stuff/, /node_modules/],
        
        loader:  'elm-webpack-loader?verbose=true&warn=true',
      },
    ],

    noParse: /\.elm$/,
  },

  plugins: [
    extractSass,
    new CleanWebpackPlugin(path.resolve(__dirname + '/dist'))
],

  devServer: {
    inline: true,
    stats: { colors: true },
  },


};
