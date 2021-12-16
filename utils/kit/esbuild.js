const {rm, mkdir} = require('fs').promises;
const esbuild = require('esbuild');

(async () => {
  await rm('./lib', {recursive: true, force: true});
  await mkdir('./lib', {recursive: true});

  esbuild
    .build({
      entryPoints: ['src/index.ts'],
      outdir: 'lib',
      bundle: true,
      sourcemap: true,
      minify: true,
      splitting: true,
      format: 'esm',
      target: ['esnext']
    })
    .catch(() => process.exit(1));
})();
