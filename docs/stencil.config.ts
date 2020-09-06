import {Config} from '@stencil/core';

import {sass} from '@stencil/sass';
import {postcss} from '@stencil/postcss';
import autoprefixer from 'autoprefixer';

export const config: Config = {
  outputTargets: [
    {
      type: 'www',
      baseUrl: 'https://docs.deckdeckgo.com',
      prerenderConfig: './prerender.config.ts',
      copy: [{src: 'robots.txt'}],
    },
  ],
  globalScript: 'src/global/app.ts',
  globalStyle: 'src/global/app.scss',
  plugins: [
    sass(),
    postcss({
      plugins: [autoprefixer()],
    }),
  ],
  nodeResolve: {browser: true},
  devServer: {
    openBrowser: false,
  },
};
