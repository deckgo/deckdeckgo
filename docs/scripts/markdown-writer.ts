import {promisify} from 'util';
import fs from 'fs';

const readFile = promisify(fs.readFile);
const writeFile = promisify(fs.writeFile);

export function updateFile(markdownHtmlContent: string, dest: string): Promise<void> {
  return new Promise<void>(async (resolve, reject) => {
    try {
      const currentContent: string = await readFile(dest, {encoding: 'utf8'});

      const updatedContent: string = currentContent.replace(/(<main>)(.*?)(<\/main>)/si, '<main>' + markdownHtmlContent + '</main>');

      await writeFile(dest, updatedContent, 'utf8');
      
      resolve();
    } catch (err) {
      reject(err);
    }
  });
}
