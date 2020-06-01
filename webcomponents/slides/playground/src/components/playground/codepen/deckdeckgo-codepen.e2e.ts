import {E2EElement, E2EPage, newE2EPage} from '@stencil/core/testing';

describe('deckgo-codepen', () => {
  let page: E2EPage;

  beforeEach(async () => {
    page = await newE2EPage();
    await page.setContent(`<deckgo-codepen height="400" src="https://codepen.io/peterpeterparker/pen/dyGbOZm" frameTitle="A pen by David"></deckgo-codepen>`);
  });

  it('renders', async () => {
    const element: E2EElement = await page.find('deckgo-codepen');
    expect(element).not.toBeNull();
    expect(element).toHaveClass('hydrated');
  });

  it('lazyLoadContent', async () => {
    const element: E2EElement = await page.find('deckgo-codepen');
    expect(element).not.toBeNull();

    let iframe: E2EElement = await page.find('deckgo-codepen >>> iframe');
    expect(iframe).toBeNull();

    await element.callMethod('lazyLoadContent');

    await page.waitForChanges();

    iframe = await page.find('deckgo-codepen >>> iframe');
    expect(iframe).not.toBeNull();
  });
});
