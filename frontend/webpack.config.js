const path = require('path');

module.exports = {
  entry: './src/index.bs.js',
  output: {
    filename: 'index.bs.js',
    path: path.resolve(__dirname, 'dist'),
  },
};
