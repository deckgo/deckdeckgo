const esbuild = require('esbuild');
const {existsSync, mkdirSync, writeFileSync} = require('fs');
const {join} = require('path');
const fs = require('fs');
const path = require('path');

const lib = join(process.cwd(), 'lib');

if (!existsSync(lib)) {
  mkdirSync(lib);
}

const findEntryPoints = (dir, files) => {
  fs.readdirSync(dir).forEach((file) => {
    const fullPath = path.join(dir, file);
    if (fs.lstatSync(fullPath).isDirectory()) {
      findEntryPoints(fullPath, files);
    } else if (path.extname(fullPath) === '.ts') {
      files.push(fullPath);
    }
  });
};

const entryPoints = [];
findEntryPoints('src', entryPoints);

esbuild
  .build({
    entryPoints,
    outdir: 'lib/esm',
    bundle: true,
    sourcemap: true,
    minify: true,
    splitting: true,
    format: 'esm',
    target: ['esnext']
  })
  .catch(() => process.exit(1));

esbuild
  .build({
    entryPoints: ['src/index.ts'],
    outfile: 'lib/cjs/index.cjs.js',
    bundle: true,
    sourcemap: true,
    minify: true,
    platform: 'node',
    target: ['node16']
  })
  .catch(() => process.exit(1));

writeFileSync(join(lib, 'index.js'), "export * from './esm/index.js';");

writeFileSync(join(lib, 'index.cjs.js'), "module.exports = require('./cjs/index.cjs.js');");
