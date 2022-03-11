import * as admin from 'firebase-admin';
import * as functions from 'firebase-functions';
import * as puppeteer from 'puppeteer';
import {Deck, DeckData} from '../../../model/data/deck';
import {findDeck} from '../../../utils/data/deck-utils';
import {Resources} from '../../../utils/resources/resources';

export async function generateDeckScreenshot(deckId: string) {
  const apiSkip: string = functions.config().deckdeckgo.api.skip;

  if (apiSkip === 'true') {
    return;
  }

  const deck: Deck = await findDeck(deckId);

  if (!deck || !deck.data) {
    console.error('Deck for screenshot not found.');
    return;
  }

  try {
    const imageBuffer: string = await generateScreenshot(deck.data);
    await saveScreenshot(deck.data, imageBuffer);
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
      const presentationUrl: string = functions.config().deckdeckgo.presentation.url;
      await page.goto(presentationUrl + pathname + '?screenshot', {waitUntil: 'networkidle0', timeout: 30000});

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

function saveScreenshot(deckData: DeckData, imageBuffer: string): Promise<void> {
  return new Promise<void>(async (resolve, reject) => {
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
