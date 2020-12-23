const fs = require('fs');

try {
  fs.copyFileSync('./src/pages/index.en.js', './src/pages/index.js');

  const content = fs.readFileSync('./src/pages/index.js');

  const comment = '/**\n * Do not modify! This file is overwritten by index.en.js at build time.\n */\n';

  fs.writeFileSync('./src/pages/index.js', comment + content);

  console.log(`index.en.js copied to index.js!`);
} catch (err) {
  console.error(`Cannot copy index.en.js`);
}
