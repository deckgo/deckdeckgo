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

export async function parseDeck(login: string, project: string) {
  // TODO use and replace real and all content
  // TODO update all files not just index.html

  const indexPath: string = getLocalIndexPath(login, project);

  const data = await fs.readFile(indexPath, 'utf8');

  let result = data.replace(/\{\{DECKDECKGO_TITLE\}\}/g, 'test');
  result = result.replace(/\{\{DECKDECKGO_AUTHOR\}\}/g, 'david');

  await fs.writeFile(indexPath, result, 'utf8');
}

export function getLocalPath(login: string, project: string): string {
  return path.join(os.tmpdir(), login, project);
}

export function getLocalIndexPath(login: string, project: string): string {
  return path.join(getLocalPath(login, project), 'src', 'index.html');
}
