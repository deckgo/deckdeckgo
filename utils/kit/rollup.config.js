import typescript from 'rollup-plugin-typescript';
import commonjs from 'rollup-plugin-commonjs';
import resolve from 'rollup-plugin-node-resolve';

import pkg from './package.json';

export default {
  input: './src/index.ts',
  plugins: [typescript(), resolve(), commonjs()],
  output: {
    format: 'cjs',
    file: pkg.main,
  },
};
