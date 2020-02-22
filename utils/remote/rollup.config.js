// Source to setup the build
// https://github.com/rollup/rollup-starter-lib
// https://buzut.net/configurer-rollup-bundles-esm-cjs/
// https://dev.to/proticm/how-to-setup-rollup-config-45mk

import typescript from 'rollup-plugin-typescript';
import commonjs from 'rollup-plugin-commonjs';

import pkg from './package.json';

export default {
  input: './src/index.ts',
  plugins: [commonjs(), typescript()],
  output: {
    format: 'cjs',
    file: pkg.main
  }
};
