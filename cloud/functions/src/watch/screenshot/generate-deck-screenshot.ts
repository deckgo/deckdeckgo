import {Change} from 'firebase-functions';
import {DocumentSnapshot} from 'firebase-functions/lib/providers/firestore';

import * as admin from 'firebase-admin';

import * as puppeteer from 'puppeteer';

import {Resources} from '../../utils/resources';

import {DeckData} from '../../model/deck';

import {isDeckPublished} from './utils/update-deck';

export async function generateDeckScreenshot(change: Change<DocumentSnapshot>) {
  const newValue: DeckData = change.after.data() as DeckData;

  const previousValue: DeckData = change.before.data() as DeckData;

  if (!newValue || !newValue.meta || !newValue.meta.published || !newValue.meta.pathname) {
    return;
  }

  if (!newValue.owner_id || newValue.owner_id === undefined || newValue.owner_id === '') {
    return;
  }

  const update: boolean = await isDeckPublished(previousValue, newValue);

  if (!update) {
    return;
  }

  try {
    const imageBuffer: string = await generateScreenshot(newValue);
    await saveScreenshot(newValue, imageBuffer);
  } catch (err) {
    console.error(err);
  }
}

function generateScreenshot(deckData: DeckData): Promise<string> {
  return new Promise<string>(async (resolve, reject) => {
    try {
      if (!deckData || !deckData.meta || !deckData.meta.pathname) {
        reject('No deck or data');
        return;
      }

      const browser = await puppeteer.launch({args: ['--no-sandbox']});

      const page = await browser.newPage();

      // Screenshot size
      await page.setViewport({width: 1024, height: 576});

      let pathname: string = deckData.meta.pathname;
      pathname += pathname.endsWith('/') ? '' : '/';

      // ?screenshot = no navigation and action displayed in the presentation
      await page.goto(Resources.Constants.PRESENTATION.URL + pathname + '?screenshot', {waitUntil: 'networkidle0', timeout: 30000});

      await (page as any)._client.send('ServiceWorker.enable');
      await (page as any)._client.send('ServiceWorker.stopAllWorkers');

      // Wait for the components/js elements to be loaded
      await page.waitForFunction('document.querySelector("deckgo-deck  > *")');

      const imageBuffer: string = await page.screenshot();

      await browser.close();

      resolve(imageBuffer);
    } catch (err) {
      reject(err);
    }
  });
}

function saveScreenshot(deckData: DeckData, imageBuffer: string): Promise<string> {
  return new Promise<string>(async (resolve, reject) => {
    if (!imageBuffer || imageBuffer === undefined || imageBuffer === '') {
      reject('No screenshot image');
      return;
    }

    if (!deckData || !deckData.meta || !deckData.meta.pathname) {
      reject('No deck or data');
      return;
    }

    const path: string[] = deckData.meta.pathname.split('/');

    if (!path || path.length < 3) {
      reject('Path not well formatted');
      return;
    }

    const bucket = admin.storage().bucket();

    // path[0] = ''
    // path[1] = user
    // path[2] = presentation-name
    const file = bucket.file(`${deckData.owner_id}/${Resources.Constants.PRESENTATION.FOLDER}/${path[2]}/${Resources.Constants.PRESENTATION.IMAGE}`);

    try {
      await file.save(imageBuffer);

      resolve();
    } catch (err) {
      reject(err);
    }
  });
}
