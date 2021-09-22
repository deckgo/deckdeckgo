#!/usr/bin/env node

const prettier = require('prettier');

const {join, extname, basename} = require('path');
const {existsSync, readdirSync, writeFileSync, lstatSync, copyFileSync, readFileSync} = require('fs');

const copyTypes = async ({src, dest = `../providers/ic/src/canisters`}) => {
  const promises = readdirSync(src)
    .filter((subFolder) => !['assets', 'idl'].includes(subFolder))
    .map(
      (dirent) =>
        new Promise(async (resolve) => {
          const [srcPath, destPath] = [src, dest].map((dirPath) => join(dirPath, dirent));

          const stat = lstatSync(srcPath);

          if (stat.isDirectory()) {
            await copyTypes({src: srcPath, dest: destPath});
          } else if (stat.isFile()) {
            await copyFile({srcPath, destPath});
          }

          resolve();
        })
    );

  await Promise.all(promises);
};

const copyFile = async ({srcPath, destPath}) => {
  if (extname(srcPath) === '.wasm') {
    return;
  }

  if (basename(srcPath) === 'index.js') {
    return;
  }

  if (extname(srcPath) === '.did') {
    copyFileSync(srcPath, destPath);
    return;
  }

  const buffer = readFileSync(srcPath);
  const config = await prettier.resolveConfig('./prettierrc');
  const output = prettier.format(buffer.toString('utf-8'), {parser: 'babel', ...config});

  writeFileSync(destPath.replace('.did.js', '.utils.did.js'), output);
};

(async () => {
  try {
    if (existsSync('.dfx/local/canisters/')) {
      await copyTypes({src: `.dfx/local/canisters/`});
    } else if (existsSync('.dfx/ic/canisters/')) {
      await copyTypes({src: `.dfx/ic/canisters/`});
    }

    console.log(`Internet Computer types declarations generated!`);
  } catch (err) {
    console.error(`Error while generating the types for the Internet Computer.`, err);
  }
})();
