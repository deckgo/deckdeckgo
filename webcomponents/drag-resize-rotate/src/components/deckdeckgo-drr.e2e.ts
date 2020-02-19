import {E2EElement, E2EPage, newE2EPage} from '@stencil/core/testing';

describe('deckgo-drr', () => {
  let page: E2EPage;

  beforeEach(async () => {
    page = await newE2EPage();
    await page.setContent(
      `<deckgo-drr width="200" height="100" top="50" left="50">
      <div style="background: red"></div>
    </deckgo-drr>`
    );
  });

  it('renders', async () => {
    const element: E2EElement = await page.find('deckgo-drr');
    expect(element).not.toBeNull();
    expect(element).toHaveClass('hydrated');
  });
});
