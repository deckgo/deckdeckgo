// https://cloud.google.com/functions/docs/concepts/exec#file_system

import {promises as fs} from 'fs';
import {html_beautify} from 'js-beautify';
import fetch, {Response} from 'node-fetch';
import * as os from 'os';
import * as path from 'path';
import * as rimraf from 'rimraf';
import {DeckMeta} from '../../../../model/data/deck';

export function deleteDir(localPath: string): Promise<void> {
  return new Promise<void>((resolve) => {
    rimraf(localPath, () => {
      resolve();
    });
  });
}

export async function parseDeck(login: string, project: string, meta: DeckMeta) {
  await parseIndexHtml(login, project, meta);
  await parseManifestJson(login, project, meta);
}

async function parseIndexHtml(login: string, project: string, meta: DeckMeta) {
  const indexPath: string = getLocalFilePath(login, project, 'src', 'index.html');

  const indexHtml: string = await getRemoteContent(meta, 'index.html');

  if (!indexHtml || indexHtml === '') {
    throw new Error('Content is empty');
  }

  // Remove unuseful </base> tag ending
  let cleanedIndexHtml: string = indexHtml.replace(/<\/base>/g, '');
  // Remove CSP to be able to run the presentation locally easily
  cleanedIndexHtml = cleanedIndexHtml.replace(/<meta http-equiv="Content-Security-Policy".*?>/g, '');
  // Remove the script of the prod build
  cleanedIndexHtml = cleanedIndexHtml.replace(/<script src=".*?>/g, '');

  let formattedHtml: string = html_beautify(cleanedIndexHtml);

  // Align code
  const matches: RegExpMatchArray | null = formattedHtml.match(/<code slot="code">([\s\S]*?)<\/code>/gm);
  if (matches && matches.length > 0) {
    matches.forEach((match) => {
      formattedHtml = formattedHtml.replace(match, match.replace(/                /g, ''));
    });
  }

  await fs.writeFile(indexPath, formattedHtml, 'utf8');
}

async function parseManifestJson(login: string, project: string, meta: DeckMeta) {
  const manifestJsonPath: string = getLocalFilePath(login, project, 'src', 'manifest.json');

  const manifestJson: string = await getRemoteContent(meta, 'manifest.json');

  await fs.writeFile(manifestJsonPath, html_beautify(manifestJson), 'utf8');
}

export async function parseInfo(login: string, project: string, url: string, meta: DeckMeta, ...files: string[]) {
  const readmePath: string = getLocalFilePath(login, project, ...files);

  const data = await fs.readFile(readmePath, 'utf8');

  let result = data.replace(/\{\{DECKDECKGO_TITLE\}\}/g, meta.title);
  result = result.replace(/\{\{DECKDECKGO_DESCRIPTION\}\}/g, meta.description ? (meta.description as string) : '');
  result = result.replace(/\{\{DECKDECKGO_BASE_HREF\}\}/g, meta.pathname);
  result = result.replace(/\{\{DECKDECKGO_GITHUB_REPO_URL\}\}/g, url);
  result = result.replace(/\{\{DECKDECKGO_GITHUB_REPO_NAME\}\}/g, project);

  await fs.writeFile(readmePath, result, 'utf8');
}

export async function shouldUpdate(login: string, project: string, ...files: string[]): Promise<boolean> {
  const readmePath: string = getLocalFilePath(login, project, ...files);
  const data = await fs.readFile(readmePath, 'utf8');
  return /\{\{DECKDECKGO_.*\}\}/g.test(data);
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
