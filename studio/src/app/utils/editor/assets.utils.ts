import {get, getMany} from 'idb-keyval';

import {Deck} from '../../models/data/deck';

import {SlotType} from '../../types/editor/slot-type';

import {deckSelector} from './deck.utils';

export const getDeckLocalImage = async (): Promise<File | undefined> => {
  return getDeckBackgroundImage();
};

export const getSlidesLocalImages = async ({deck}: {deck: Deck}): Promise<File[]> => {
  return getAssets({deck, getAssets: getSlideLocalImages});
};

export const getSlidesLocalCharts = async ({deck}: {deck: Deck}): Promise<File[]> => {
  return getAssets({deck, getAssets: getSlideLocalCharts});
};

const getAssets = async ({deck, getAssets}: {deck: Deck; getAssets: ({slideId}: {slideId: string}) => Promise<File[] | undefined>}): Promise<File[]> => {
  if (!deck.data.slides || deck.data.slides.length <= 0) {
    return [];
  }

  try {
    const data: (File[] | undefined)[] = await Promise.all(deck.data.slides.map((slideId: string) => getAssets({slideId})));
    return [].concat(...data.filter((files: File[] | undefined) => files?.length));
  } catch (err) {
    throw new Error('Error while getting slides images');
  }
};

const getDeckBackgroundImage = async (): Promise<File | undefined> => {
  const backgroundElement: HTMLElement = document.querySelector(`${deckSelector} > *[slot="background"]`);

  if (!backgroundElement) {
    return undefined;
  }

  const img: HTMLDeckgoLazyImgElement = backgroundElement.querySelector(SlotType.IMG);

  if (!img) {
    return undefined;
  }

  if (!isLocalImage(img)) {
    return undefined;
  }

  const {imgSrc} = img;

  return get<File>(imgSrc);
};

const getSlideLocalImages = async ({slideId}: {slideId: string}): Promise<File[] | undefined> => {
  const slideElement: HTMLElement = document.querySelector(`${deckSelector} > *[slide_id="${slideId}"]`);

  if (!slideElement) {
    return undefined;
  }

  const imgs: NodeListOf<HTMLDeckgoLazyImgElement> = slideElement.querySelectorAll(SlotType.IMG);

  if (!imgs || imgs.length <= 0) {
    return undefined;
  }

  // Filter online images (http...) and deck background (which are cloned from the deck to the slides)
  const list: HTMLDeckgoLazyImgElement[] = Array.from(imgs).filter((img: HTMLDeckgoLazyImgElement) => {
    return isLocalImage(img) && !(img.parentElement?.getAttribute('slot') === 'background' && !slideElement.hasAttribute('custom-background'));
  });

  return getMany<File>(list.map(({imgSrc}: HTMLDeckgoLazyImgElement) => imgSrc));
};

const isLocalImage = ({imgSrc}: HTMLDeckgoLazyImgElement): boolean => imgSrc !== undefined && imgSrc !== '' && !imgSrc.startsWith('http');

const getSlideLocalCharts = async ({slideId}: {slideId: string}): Promise<File[] | undefined> => {
  const slideElement: HTMLElement = document.querySelector(`${deckSelector} > *[slide_id="${slideId}"]`);

  if (!slideElement) {
    return undefined;
  }

  if (slideElement.tagName && slideElement.tagName.toUpperCase() !== 'deckgo-slide-chart'.toUpperCase()) {
    return undefined;
  }

  const src: string = (slideElement as HTMLDeckgoSlideChartElement).src;

  if (!src || src === undefined || src === '') {
    return undefined;
  }

  return getMany<File>([src]);
};
