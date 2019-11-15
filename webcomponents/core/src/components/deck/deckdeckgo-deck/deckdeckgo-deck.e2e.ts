import { newE2EPage } from '@stencil/core/testing';

describe('deckgo-deck', () => {
  it('renders', async () => {
    const page = await newE2EPage();

    await page.setContent('<deckgo-deck></deckgo-deck>');
    const element = await page.find('deckgo-deck');
    expect(element).toHaveClass('hydrated');
  });
});
