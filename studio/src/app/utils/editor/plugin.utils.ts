import {StorageFile, UnsplashPhoto} from '@deckdeckgo/editor';
import {createEmptyElement, transformParagraph} from '@deckdeckgo/stylo';

import {SlotType} from '../../types/editor/slot-type';

import {initDeckgoLazyImgAttributes} from './image.utils';

export const createParagraphImage = ({
  image,
  paragraph,
  container
}: {
  image: UnsplashPhoto | TenorGif | StorageFile;
  paragraph: HTMLElement;
  container: HTMLElement;
}) => {
  const deckgoImg: HTMLDeckgoLazyImgElement = document.createElement(SlotType.IMG);

  const img: HTMLDeckgoLazyImgElement = initDeckgoLazyImgAttributes({
    element: deckgoImg,
    image
  });

  const emptyDiv: HTMLElement = createEmptyElement({nodeName: 'div'});

  transformParagraph({
    elements: [img, emptyDiv],
    paragraph,
    container,
    focus: 'last'
  });
};
