import {newE2EPage} from '@stencil/core/testing';

describe('social-img', () => {
  const text = 'Hello World';
  const logo = 'https://deckdeckgo.com/assets/img/deckdeckgo-logo.svg';

  it('renders', async () => {
    const page = await newE2EPage();

    await page.setContent('<deckgo-social-img></deckgo-social-img>');
    const element = await page.find('deckgo-social-img');
    expect(element).toHaveClass('hydrated');
  });

  it('renders a shadowed svg', async () => {
    const page = await newE2EPage();

    await page.setContent('<deckgo-social-img></deckgo-social-img>');
    const element = await page.find('deckgo-social-img >>> svg');
    expect(element).not.toBeNull();
  });

  it('renders a foreign object width with paddings', async () => {
    const page = await newE2EPage();

    await page.setContent(`<deckgo-social-img text="${text}"></deckgo-social-img>`);
    const element = await page.find('deckgo-social-img >>> foreignObject');
    expect(element).not.toBeNull();
    expect(parseInt(element.getAttribute('width'))).toEqual(1088);
  });

  it('renders a foreign object width with paddings minus space for logo', async () => {
    const page = await newE2EPage();

    await page.setContent(`<deckgo-social-img text="${text}" img-src="${logo}"></deckgo-social-img>`);
    const element = await page.find('deckgo-social-img >>> foreignObject');
    expect(element).not.toBeNull();
    expect(parseInt(element.getAttribute('width'))).toEqual(912);
  });
});
