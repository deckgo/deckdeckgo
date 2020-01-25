import {E2EElement, E2EPage, newE2EPage} from '@stencil/core/testing';

describe('deckgo-youtube', () => {
  let page: E2EPage;

  beforeEach(async () => {
    page = await newE2EPage();
    await page.setContent(
      `<deckgo-youtube width="500" height="400" src="https://www.youtube.com/embed/Y97mEj9ZYmE" frameTitle="DeckDeckGo editor demo"></deckgo-youtube>`
    );
  });

  it('renders', async () => {
    const element: E2EElement = await page.find('deckgo-youtube');
    expect(element).not.toBeNull();
    expect(element).toHaveClass('hydrated');
  });

  it('lazyLoadContent', async () => {
    const element: E2EElement = await page.find('deckgo-youtube');
    expect(element).not.toBeNull();

    let iframe: E2EElement = await page.find('deckgo-youtube >>> iframe');
    expect(iframe).toBeNull();

    await element.callMethod('lazyLoadContent');

    await page.waitForChanges();

    iframe = await page.find('deckgo-youtube >>> iframe');
    expect(iframe).not.toBeNull();
  });
});
