import {E2EElement, E2EPage, newE2EPage} from '@stencil/core/testing';

describe('deckgo-demo', () => {
  let page: E2EPage;

  beforeEach(async () => {
    page = await newE2EPage();
    await page.setContent(`<deckgo-demo src="https://demo.deckdeckgo.com"></deckgo-demo>`);
  });

  it('renders', async () => {
    const element: E2EElement = await page.find('deckgo-demo');
    expect(element).not.toBeNull();
    expect(element).toHaveClass('hydrated');
  });

  it('lazyLoadContent', async () => {
    const element: E2EElement = await page.find('deckgo-demo');
    expect(element).not.toBeNull();

    let iframe: E2EElement = await page.find('deckgo-demo >>> iframe');
    expect(iframe).toBeNull();

    await element.callMethod('lazyLoadContent');

    await page.waitForChanges();

    iframe = await page.find('deckgo-demo >>> iframe');
    expect(iframe).not.toBeNull();
  });
});
