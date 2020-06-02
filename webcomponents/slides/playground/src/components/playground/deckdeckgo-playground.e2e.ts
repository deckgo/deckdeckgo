import {E2EElement, E2EPage, newE2EPage} from '@stencil/core/testing';

describe('deckgo-playground', () => {
  let page: E2EPage;

  beforeEach(async () => {
    page = await newE2EPage();
    await page.setContent(
      `<deckgo-playground height="400" src="https://codepen.io/peterpeterparker/pen/dyGbOZm" frameTitle="A pen by David"></deckgo-playground>`
    );
  });

  it('renders', async () => {
    const element: E2EElement = await page.find('deckgo-playground');
    expect(element).not.toBeNull();
    expect(element).toHaveClass('hydrated');
  });

  it('lazyLoadContent', async () => {
    const element: E2EElement = await page.find('deckgo-playground');
    expect(element).not.toBeNull();

    let iframe: E2EElement = await page.find('deckgo-playground >>> iframe');
    expect(iframe).toBeNull();

    await element.callMethod('lazyLoadContent');

    await page.waitForChanges();

    iframe = await page.find('deckgo-playground >>> iframe');
    expect(iframe).not.toBeNull();
  });
});
