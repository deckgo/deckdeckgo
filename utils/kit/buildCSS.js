#!/usr/bin/env node

const {writeFile, mkdir, rm, readdir} = require('fs').promises;
const path = require('path');

const {renderSync} = require('node-sass');
const autoprefixer = require('autoprefixer');
const postcss = require('postcss');
const CleanCSS = require('clean-css');

const generateCSS = async ({fileName}) => {
  const {css, map} = renderSync({
    file: `./styles/${fileName}.scss`,
    outFile: `./css/${fileName}.css`,
    outputStyle: 'expanded',
    sourceMap: true
  });

  const {css: prefixedCSS} = await postcss([autoprefixer]).process(css, {
    from: undefined,
    to: `./css/${fileName}.css`,
    map: false
  });

  await writeFile(`./css/${fileName}.css`, prefixedCSS);
  await writeFile(`./css/${fileName}.css.map`, map);

  const {styles: minifyCSS} = new CleanCSS().minify(prefixedCSS);

  await writeFile(`./css/${fileName}.min.css`, minifyCSS);
};

(async () => {
  try {
    await rm('./css', {recursive: true, force: true});
    await mkdir('./css', {recursive: true});

    const files = await readdir('./styles/');

    const promises = files.map((file) => generateCSS({fileName: path.parse(file).name}));
    await Promise.all(promises);

    console.log(`CSS generated!`);
  } catch (err) {
    console.error(`Cannot generate CSS.`, err);
  }
})();
