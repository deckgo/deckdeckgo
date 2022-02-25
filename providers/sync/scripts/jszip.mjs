#!/usr/bin/env node

import {writeFile} from 'fs/promises';
import fetch from 'node-fetch';
import {join} from 'path';

const downloadTypeScriptDefinition = async () => {
  const response = await fetch(`https://raw.githubusercontent.com/Stuk/jszip/master/index.d.ts`);

  if (!response || !response.ok) {
    throw new Error('jszip TypeScript definition cannot be fetched');
  }

  const content = await response.text();

  return content;
};

const download = async () => {
  const content = await downloadTypeScriptDefinition();

  const dir = join(process.cwd(), 'src', 'types');

  await writeFile(`${dir}/jszip.d.ts`, content.replace('export = JSZip;', ''), 'utf-8');
};

await download();
