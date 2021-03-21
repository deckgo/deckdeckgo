import * as admin from 'firebase-admin';

import {v4 as uuid} from 'uuid';
import {Entry, Parse} from 'unzipper';

import {Metadata, MetadataSlide} from '../types/metadata';

import {File} from '@google-cloud/storage';

export async function unzipDeck(objName: string) {
  const bucket = admin.storage().bucket();
  const file: File = bucket.file(objName);

  const meta: string | undefined = await readMeta(file);

  if (!meta) {
    return;
  }

  await unzip(objName, file, meta);

  await file.delete();
}

const readMeta = (file: File): Promise<string | undefined> => {
  const stream = file.createReadStream().pipe(Parse());

  let meta: string | undefined = undefined;

  return new Promise<string | undefined>((resolve, reject) => {
    stream.on('entry', async (entry: Entry) => {
      if (entry.path === 'meta.json') {
        meta = (await entry.buffer()).toString();
      } else {
        entry.autodrain();
      }
    });
    stream.on('finish', () => resolve(meta));
    stream.on('error', (error: Error) => reject(error));
  });
};

const unzip = (objName: string, file: File, meta: string): Promise<void> => {
  const userId: string = objName.split('/')[0];
  const dataId: string = uuid();

  const metadata: Metadata = JSON.parse(meta);
  const background: string[] = metadata.slides.map((filteredMetadata: MetadataSlide) => filteredMetadata.background);

  const stream = file.createReadStream().pipe(Parse());

  const bucket = admin.storage().bucket();

  return new Promise<void>((resolve, reject) => {
    stream.on('entry', (entry: Entry) => {
      // We want to store the backgrounds as persistent images and the text as temporary data
      const assetsPath: string = background.includes(entry.path) ? 'images' : 'decks';

      const destination = bucket.file(`${userId}/assets/${assetsPath}/${dataId}/${entry.path}`);
      return entry.pipe(destination.createWriteStream());
    });
    stream.on('finish', () => resolve());
    stream.on('error', (error: Error) => reject(error));
  });
};
