import {E2EElement, E2EPage, newE2EPage} from '@stencil/core/testing';

describe('deckgo-social', () => {
  let page: E2EPage;

  beforeEach(async () => {
    page = await newE2EPage();
    await page.setContent(`<div>
    <deckgo-social twitter="daviddalbusco">
      <span slot="icon">icon</span>
      twitter
    </deckgo-social>
  </div>
  <div><deckgo-social linkedin="david-dal-busco/">linkedin/david-dal-busco</deckgo-social></div>
  <div><deckgo-social medium="daviddalbusco"></deckgo-social></div>
  <div><deckgo-social dev="daviddalbusco"></deckgo-social></div>
  <div><deckgo-social github="peterpeterparker">+daviddalbusco</deckgo-social></div>
  <div><deckgo-social full-url="https://daviddalbusco.com">https://daviddalbusco.com</deckgo-social></div>`);
  });

  it('renders', async () => {
    const elements: E2EElement[] = await page.findAll('deckgo-social');
    expect(elements).not.toBeNull();
    expect(elements).toHaveLength(6);

    const hydratedElements: E2EElement[] = elements.filter((element: E2EElement) => {
      return element.classList && element.classList.contains('hydrated');
    });

    expect(hydratedElements).not.toBeNull();
    expect(hydratedElements).toHaveLength(6);
  });

  it('rendersIcon', async () => {
    const element: E2EElement = await page.find('div:nth-of-type(1) deckgo-social');
    expect(element).not.toBeNull();

    let icon: E2EElement = await page.find('div:nth-of-type(1) deckgo-social >>> slot[name="icon"]');
    expect(icon).not.toBeNull();
  });

  it('rendersAutomaticHandle', async () => {
    const element: E2EElement = await page.find('div:nth-of-type(3) deckgo-social');
    expect(element).not.toBeNull();

    let span: E2EElement = await page.find('div:nth-of-type(3) deckgo-social >>> span');
    expect(span).not.toBeNull();
    expect(span.innerHTML).toEqual('daviddalbusco');
  });

  it('rendersCustomHandle', async () => {
    const element: E2EElement = await page.find('div:nth-of-type(5) deckgo-social');
    expect(element).not.toBeNull();
    expect(element.textContent).toEqual('+daviddalbusco');
  });
});
