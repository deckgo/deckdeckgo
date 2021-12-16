#!/usr/bin/env node

const {writeFile, mkdir, rm} = require('fs').promises;

const {renderSync} = require('node-sass');
const autoprefixer = require('autoprefixer');
const postcss = require('postcss');
const CleanCSS = require('clean-css');

(async () => {
  try {
    await rm('./css', {recursive: true, force: true});
    await mkdir('./css', {recursive: true});

    const {css, map} = renderSync({
      file: './styles/index.scss',
      outFile: './css/kit.css',
      outputStyle: 'expanded',
      sourceMap: true
    });

    const {css: prefixedCSS} = await postcss([autoprefixer]).process(css, {
      from: undefined,
      to: './css/kit.css',
      map: false
    });

    await writeFile('./css/kit.css', prefixedCSS);
    await writeFile('./css/kit.css.map', map);

    const {styles: minifyCSS} = new CleanCSS().minify(prefixedCSS);

    await writeFile('./css/kit.min.css', minifyCSS);

    console.log(`CSS generated!`);
  } catch (err) {
    console.error(`Cannot generate CSS.`, err);
  }
})();
