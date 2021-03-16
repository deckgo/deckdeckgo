import * as functions from 'firebase-functions';
import * as admin from 'firebase-admin';

import {v4 as uuid} from 'uuid';

import {Parse} from 'unzipper';

import {createDeck} from './utils/create-deck';

export async function applyWatchImportDeck(obj: functions.storage.ObjectMetadata) {
  if (!obj.bucket || !obj.name) {
    return;
  }

  const objName: string = obj.name;

  try {
    if (objName.endsWith('zip') && objName.indexOf('/assets/decks/')) {
      await unzipDeck(objName);
    }

    if (/.*\/assets\/decks\/.*\/meta.json/g.test(objName)) {
      await createDeck(objName);
    }
  } catch (err) {
    console.error(err);
  }
}

async function unzipDeck(objName: string) {
  const bucket = admin.storage().bucket();

  const userId: string = objName.split('/')[0];
  const dataId: string = uuid();

  const file = bucket.file(objName);

  const unzip = (): Promise<void> => {
    const stream = file.createReadStream().pipe(Parse());

    return new Promise<void>((resolve, reject) => {
      stream.on('entry', (entry) => {
        const destination = bucket.file(`${userId}/assets/decks/${dataId}/${entry.path}`);
        return entry.pipe(destination.createWriteStream());
      });
      stream.on('finish', () => resolve());
      stream.on('error', (error: Error) => reject(error));
    });
  };

  await unzip();

  await file.delete();
}
