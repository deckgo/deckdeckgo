import {Config} from '@stencil/core';

import {sass} from '@stencil/sass';
import {postcss} from '@stencil/postcss';
import autoprefixer from 'autoprefixer';

export const config: Config = {
  outputTargets: [{
    type: 'www'
  }],
  globalScript: 'src/global/app.ts',
  globalStyle: 'src/global/app.scss',
  plugins: [
    sass(),
    postcss({
      plugins: [autoprefixer()]
    })
  ],
  nodeResolve: {browser: true},
  devServer: {
    openBrowser: false
  },
  copy: [
    {src: 'robots.txt'}
  ]
};
