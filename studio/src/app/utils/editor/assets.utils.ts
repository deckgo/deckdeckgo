import {getMany} from 'idb-keyval';

import {Deck} from '../../models/data/deck';

import {SlotType} from '../../types/editor/slot-type';

import {deckSelector} from './deck.utils';

export const getSlidesLocalImages = async ({deck}: {deck: Deck}): Promise<File[]> => {
  if (!deck.data.slides || deck.data.slides.length <= 0) {
    return [];
  }

  try {
    const data: (File[] | undefined)[] = await Promise.all(deck.data.slides.map((slideId: string) => getSlideLocalImages({slideId})));
    return [].concat(...data.filter((files: File[] | undefined) => files?.length));
  } catch (err) {
    throw new Error('Error while getting slides images');
  }
};

export const getSlideLocalImages = async ({slideId}: {slideId: string}): Promise<File[] | undefined> => {
  const slideElement: HTMLElement = document.querySelector(`${deckSelector} > *[slide_id="${slideId}"]`);

  if (!slideElement) {
    return undefined;
  }

  const imgs: NodeListOf<HTMLDeckgoLazyImgElement> = slideElement.querySelectorAll(SlotType.IMG);

  if (!imgs || imgs.length <= 0) {
    return undefined;
  }

  // Filter online images (http...) and deck background (which are cloned from the deck to the slides)
  const list: HTMLDeckgoLazyImgElement[] = Array.from(imgs).filter(({imgSrc, parentElement}: HTMLDeckgoLazyImgElement) => {
    return (
      imgSrc !== undefined &&
      imgSrc !== '' &&
      !imgSrc.startsWith('http') &&
      !(parentElement?.getAttribute('slot') === 'background' && !slideElement.hasAttribute('custom-background'))
    );
  });

  return getMany<File>(list.map(({imgSrc}: HTMLDeckgoLazyImgElement) => imgSrc));
};
