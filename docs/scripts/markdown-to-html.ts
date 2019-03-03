import glob from 'glob';
import {promisify} from 'util';
import fs from 'fs';
import path from 'path';

import marked from 'marked';
import {changeCodeCreation} from './markdown-renderer';
import {updateFile} from './markdown-writer';

const readFile = promisify(fs.readFile);
const globAsync = promisify(glob);

(async function () {
  console.log('> Running markdown to Html 🐶');

  const tsxFiles: string[] = await globAsync('./src/**/*.tsx', {});
  const markdownFiles: string[] = await globAsync('./docs/**/*.md', {});

  if (!tsxFiles && tsxFiles.length <= 0) {
    console.log('> No components found 🙈');
    return;
  }

  if (!markdownFiles && markdownFiles.length <= 0) {
    console.log('> No markdown files found 🙈');
    return;
  }

  const filePromises = markdownFiles.map(async (file: string) => {
    const dest: string = file.replace('./docs/', './src/app/pages/docs/').replace('.md', '.tsx');

    if (tsxFiles.indexOf(dest) === -1) {
      return Promise.resolve();
    }

    try {
      const markdownContents: string = await readFile(file, {encoding: 'utf8'});

      const renderer = new marked.Renderer();
      changeCodeCreation(renderer);

      const fileName: string = path.basename(file, path.extname(file));

      const markdownHtmlContents: string = marked(markdownContents, {
        renderer,
        headerIds: true,
        headerPrefix: fileName + '-',
        xhtml: true
      });

      await updateFile(markdownHtmlContents, dest);
    } catch (err) {
      console.error(file);
      throw err;
    }

  });

  await Promise.all(filePromises);

  console.log(`> Documentation successfully generated, ${filePromises.length} files converted 🚀\n`);
})();
