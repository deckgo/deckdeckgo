// https://cloud.google.com/functions/docs/concepts/exec#file_system

import * as rimraf from 'rimraf';

import {promises as fs} from 'fs';
import * as os from 'os';
import * as path from 'path';

export function deleteDir(localPath: string): Promise<void> {
  return new Promise<void>((resolve) => {
    rimraf(localPath, () => {
      resolve();
    });
  });
}

export function parseDeck(): Promise<void> {
  return new Promise<void>(async (resolve, reject) => {
    try {
      // TODO use and replace real and all content
      // TODO update all files not just index.html

      //  TODO replace test with project name
      const localPath: string = path.join(os.tmpdir(), 'test');

      const indexPath: string = path.join(localPath, 'src', 'index.html');

      const data = await fs.readFile(indexPath, 'utf8');

      let result = data.replace(/\{\{DECKDECKGO_TITLE\}\}/g, 'test');
      result = result.replace(/\{\{DECKDECKGO_AUTHOR\}\}/g, 'david');

      await fs.writeFile(indexPath, result, 'utf8');

      resolve();
    } catch (err) {
      reject(err);
    }
  });
}
