import {DocLazyImage} from './Readme.mdx';

export default {
  title: 'Components/Lazy Image',
  parameters: {
    docs: {
      page: DocLazyImage
    }
  },
  argTypes: {
    imgSrc: {control: 'text'},
    imgAlt: {control: 'text'},
    observerRootMargin: {control: 'text'},
    observerThreshold: {control: 'number'},
    ariaLabel: {control: 'text'}
  }
};

export const LazyImage = ({imgSrc, imgAlt, observerRootMargin, observerThreshold, ariaLabel}) => {
  return `<deckgo-lazy-img img-src="${imgSrc}" img-alt="${imgAlt}" observer-root-margin="${observerRootMargin}"
                           observer-threshold="${observerThreshold}"
                           aria-label="${ariaLabel}">
</deckgo-lazy-img>`;
};

LazyImage.args = {
  imgSrc: 'https://deckdeckgo.com/assets/deckdeckgo.png',
  imgAlt: 'DeckDeckGo',
  ariaLabel: 'DeckDeckGo',
  observerRootMargin: '300px',
  observerThreshold: 0.25
};
