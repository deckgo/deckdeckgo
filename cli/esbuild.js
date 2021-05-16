const esbuild = require('esbuild');
const {writeFile, mkdir} = require('fs').promises;

const script = esbuild
  .buildSync({
      entryPoints: ['src/index.ts'],
      bundle: true,
      minify: true,
      platform: 'node',
      write: false,
      target: ['node14']
  });

(async () => {
      await mkdir('dist');

      await writeFile(
        'dist/index.js',
        `#!/usr/bin/env node\n${script.outputFiles[0].text}`
      );
})();
