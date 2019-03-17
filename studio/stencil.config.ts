import {Config} from '@stencil/core';

import {sass} from '@stencil/sass';
import {postcss} from '@stencil/postcss';
import autoprefixer from 'autoprefixer';

let globalScript: string = 'src/global/app.ts';

// @ts-ignore
const dev: boolean = process.argv && process.argv.indexOf('--dev') > -1;
if (dev) {
    globalScript = 'src/global/app-dev.ts';
}

export const config: Config = {
    outputTargets: [{
        type: 'www',
        serviceWorker: null
    }],
    globalScript: globalScript,
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
