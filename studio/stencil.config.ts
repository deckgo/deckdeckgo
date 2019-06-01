import {Config} from '@stencil/core';

import {sass} from '@stencil/sass';
import {postcss} from '@stencil/postcss';
import autoprefixer from 'autoprefixer';

import replace from 'rollup-plugin-replace';

// @ts-ignore
const dev: boolean = process.argv && process.argv.indexOf('--dev') > -1;

const globalScript: string = dev ? 'src/global/app-dev.ts' : 'src/global/app.ts';

// @ts-ignore
import devConfig from './config.dev.json';
// @ts-ignore
import prodConfig from './config.prod.json';

const configValues = dev ? devConfig : prodConfig;

export const config: Config = {
    outputTargets: [{
        type: 'www'
    }],
    globalScript: globalScript,
    globalStyle: 'src/global/app.scss',
    plugins: [
        replace({
            exclude: 'node_modules/**',
            delimiters: ['<@', '@>'],
            values: configValues
        }),
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
