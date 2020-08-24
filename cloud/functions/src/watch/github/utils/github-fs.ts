// https://cloud.google.com/functions/docs/concepts/exec#file_system

import fetch, {Response} from 'node-fetch';

import * as rimraf from 'rimraf';

import {html_beautify} from 'js-beautify';

import {promises as fs} from 'fs';
import * as os from 'os';
import * as path from 'path';

import {DeckMeta} from '../../../model/deck';

export function deleteDir(localPath: string): Promise<void> {
  return new Promise<void>((resolve) => {
    rimraf(localPath, () => {
      resolve();
    });
  });
}

export async function parseDeck(login: string, project: string, meta: DeckMeta) {
  // TODO use and replace real and all content
  // TODO update all files not just index.html

  await parseIndexHtml(login, project, meta);
  await parseManifestJson(login, project, meta);
}

async function parseIndexHtml(login: string, project: string, meta: DeckMeta) {
  const indexPath: string = getLocalFilePath(login, project, 'src', 'index.html');

  const indexHtml: string = await getRemoteContent(meta, 'index.html');

  await fs.writeFile(indexPath, html_beautify(indexHtml), 'utf8');
}

async function parseManifestJson(login: string, project: string, meta: DeckMeta) {
  const manifestJsonPath: string = getLocalFilePath(login, project, 'src', 'manifest.json');

  const manifestJson: string = await getRemoteContent(meta, 'manifest.json');

  await fs.writeFile(manifestJsonPath, html_beautify(manifestJson), 'utf8');
}

async function getRemoteContent(meta: DeckMeta, rootFilename: string): Promise<string> {
  try {
    const response: Response = await fetch(`https://beta.deckdeckgo.io${meta.pathname}${rootFilename}`);

    if (!response || !response.ok) {
      console.error(response);
      throw new Error('Cannot fetch deck index HTML.');
    }

    const html: string = await response.text();

    return html;
  } catch (err) {
    console.error(err);
    throw new Error('Unexpected error while trying to fetch deck index HTML.');
  }
}

export function getLocalPath(login: string, project: string): string {
  return path.join(os.tmpdir(), login, project);
}

export function getLocalFilePath(login: string, project: string, ...files: string[]): string {
  return path.join(getLocalPath(login, project), ...files);
}
