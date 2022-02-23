const esbuild = require('esbuild');
const {readdirSync, existsSync, mkdirSync, writeFileSync, lstatSync} = require('fs');
const {join} = require('path');
const path = require('path');

const lib = join(process.cwd(), 'lib');

if (!existsSync(lib)) {
  mkdirSync(lib);
}

const src = join(process.cwd(), 'src');

const entryPoints = readdirSync(src)
  .filter((file) => {
    const fullPath = path.join(src, file);
    return !lstatSync(fullPath).isDirectory();
  })
  .map((file) => `src/${file}`);

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

writeFileSync(join(lib, 'index.js'), "export * from './esm/index.js';");
