const esbuild = require('esbuild');

esbuild
  .build({
      entryPoints: ['src/index.ts'],
      bundle: true,
      minify: true,
      platform: 'node',
      target: ['node10.4'],
      outfile: 'dist/index.js'
  })
  .catch(() => process.exit(1));

