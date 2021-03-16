import * as functions from 'firebase-functions';

import {createDeck} from './utils/create-deck';
import {unzipDeck} from './utils/unzip-deck';

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
