import {newSpecPage} from '@stencil/core/testing';

import {SocialImg} from './social-img';

describe('social-image', () => {
  const text = 'Hello World';
  const logo = 'https://deckdeckgo.com/assets/img/deckdeckgo-logo.svg';

  it('renders', async () => {
    const {root} = await newSpecPage({
      components: [SocialImg],
      html: '<deckgo-social-img></deckgo-social-img>'
    });

    expect(root).toEqualHtml(`
      <deckgo-social-img>
        <mock:shadow-root>
          <svg height="628px" width="1200px" x="0" xmlns="http://www.w3.org/2000/svg" y="0">
            <rect fill="#3dc2ff" height="500" rx="0" ry="0" stroke="#3dc2ff" stroke-width="5" width="1072" x="80" y="80"></rect>
            <rect fill="white" height="500" rx="0" ry="0" stroke="#3dc2ff" stroke-width="5" width="1072" x="64" y="64"></rect>
          </svg>
        </mock:shadow-root>
      </deckgo-social-img>
    `);
  });

  it('renders a text', async () => {
    const {root} = await newSpecPage({
      components: [SocialImg],
      html: `<deckgo-social-img text="${text}"></deckgo-social-img>`
    });

    expect(root).toEqualHtml(`
      <deckgo-social-img text="${text}">
        <mock:shadow-root>
          <svg height="628px" width="1200px" x="0" xmlns="http://www.w3.org/2000/svg" y="0">
            <rect fill="#3dc2ff" height="500" rx="0" ry="0" stroke="#3dc2ff" stroke-width="5" width="1072" x="80" y="80"></rect>
            <rect fill="white" height="500" rx="0" ry="0" stroke="#3dc2ff" stroke-width="5" width="1072" x="64" y="64"></rect>
            <foreignObject height="436" width="1008" x="96" y="96"><p part="text">${text}</p></foreignObject>
          </svg>
        </mock:shadow-root>
      </deckgo-social-img>
    `);
  });

  it('renders a logo', async () => {
    const {root} = await newSpecPage({
      components: [SocialImg],
      html: `<deckgo-social-img img-src="${logo}"></deckgo-social-img>`
    });

    expect(root).toEqualHtml(`
      <deckgo-social-img img-src="${logo}">
        <mock:shadow-root>
          <svg height="628px" width="1200px" x="0" xmlns="http://www.w3.org/2000/svg" y="0">
            <rect fill="#3dc2ff" height="500" rx="0" ry="0" stroke="#3dc2ff" stroke-width="5" width="1072" x="80" y="80"></rect>
            <rect fill="white" height="500" rx="0" ry="0" stroke="#3dc2ff" stroke-width="5" width="1072" x="64" y="64"></rect>
            <image height="64" href="${logo}" part="img" width="64" x="1040" y="468"></image>
          </svg>
        </mock:shadow-root>
      </deckgo-social-img>
    `);
  });

  it('renders a text and logo', async () => {
    const {root} = await newSpecPage({
      components: [SocialImg],
      html: `<deckgo-social-img text="${text}" img-src="${logo}"></deckgo-social-img>`
    });

    expect(root).toEqualHtml(`
      <deckgo-social-img text="${text}" img-src="${logo}">
        <mock:shadow-root>
          <svg height="628px" width="1200px" x="0" xmlns="http://www.w3.org/2000/svg" y="0">
            <rect fill="#3dc2ff" height="500" rx="0" ry="0" stroke="#3dc2ff" stroke-width="5" width="1072" x="80" y="80"></rect>
            <rect fill="white" height="500" rx="0" ry="0" stroke="#3dc2ff" stroke-width="5" width="1072" x="64" y="64"></rect>
            <foreignObject height="436" width="912" x="96" y="96"><p part="text">${text}</p></foreignObject>
            <image height="64" href="${logo}" part="img" width="64" x="1040" y="468"></image>
          </svg>
        </mock:shadow-root>
      </deckgo-social-img>
    `);
  });
});
