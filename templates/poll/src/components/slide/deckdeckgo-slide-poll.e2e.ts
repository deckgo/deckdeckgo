import {E2EElement, E2EPage, newE2EPage} from '@stencil/core/testing';

describe('deckgo-slide-poll', () => {
  let page: E2EPage;

  beforeEach(async () => {
    page = await newE2EPage();
    await page.setContent(`<deckgo-slide-poll style="--deckgo-qrcode-color-fill: yellow; --deckgo-chart-fill-color-1: red;">
    <h1 slot="question">Do you like my presentation so far?</h1>
    <p slot="answer-1">It is super</p>
    <p slot="answer-2">Meh</p>
    <p slot="answer-3">I could'nt care less</p>
    <p slot="answer-4">Tell me why</p>
    <p slot="how-to">Go to <a href="https://deckdeckgo.com/poll">deckdeckgo.com/poll</a> and use the code {0}</p>
    <p slot="awaiting-votes">Awaiting first votes</p>
    <p slot="answer-5">Ain't nothin' but a heartache</p>
  </deckgo-slide-poll>`);
  });

  it('renders', async () => {
    const element: E2EElement = await page.find('deckgo-slide-poll');
    expect(element).not.toBeNull();
    expect(element).toHaveClass('hydrated');
  });

  it('lazyLoadContent', async () => {
    const element: E2EElement = await page.find('deckgo-slide-poll');
    expect(element).not.toBeNull();

    let chart: E2EElement = await page.find('deckgo-slide-poll >>> deckgo-bar-chart');
    expect(chart).toBeNull();

    await element.callMethod('lazyLoadContent');

    await page.waitForChanges();

    chart = await page.find('deckgo-slide-poll >>> deckgo-bar-chart');
    expect(chart).not.toBeNull();
  });

  it('socketPath', async () => {
    const element: E2EElement = await page.find('deckgo-slide-poll');
    expect(element).not.toBeNull();

    const prop: string = await element.getProperty('socketPath');

    expect(prop).toBe('/poll');
  });

  it('qrCode', async () => {
    const element: E2EElement = await page.find('deckgo-slide-poll');
    expect(element).not.toBeNull();

    await page.$eval('deckgo-slide-poll', (elm: any) => {
      elm.pollLink = 'https://deckdeckgo.com/poll';
    });

    await page.waitForChanges();

    const prop: string = await element.getProperty('pollLink');

    expect(prop).toBe('https://deckdeckgo.com/poll');

    const qrCode: E2EElement = await page.find('deckgo-slide-poll >>> deckgo-qrcode');
    expect(qrCode).not.toBeNull();
  });
});
