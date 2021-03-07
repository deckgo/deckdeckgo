import fetch, {Response} from 'node-fetch';

import * as puppeteer from 'puppeteer';

export async function createDeck() {
  const response: Response = await fetch(`https://raw.githubusercontent.com/deckgo/deckdeckgo/figma/webcomponents/slides/svg/showcase/slide-with-text.svg`);

  if (!response || !response.ok) {
    console.error(response);
    return;
  }

  const svg = await response.text();

  const browser = await puppeteer.launch({args: ['--no-sandbox']});

  const page = await browser.newPage();

  // Screenshot size
  await page.setViewport({width: 1980, height: 1080});

  await page.setContent(`${svg}`, {waitUntil: 'domcontentloaded'});

  // await page.pdf({ path: 'html.pdf', format: 'A4' })

  // const text = await page.evaluate(() =>  document.querySelector('span'));
  const text = await page.mainFrame().$$('text');

  console.log(await text?.[0].boundingBox());

  const label = await page.evaluate((el) => el.outerHTML, text?.[0]);

  console.log(label);

  await browser.close();
}

(async () => {
  await createDeck();
})();
