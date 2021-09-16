import {v4 as uuid} from 'uuid';

import {get, getMany} from 'idb-keyval';

import {Deck} from '@deckdeckgo/editor';

import {SlotType} from '../../types/editor/slot-type';

import {deckSelector} from './deck.utils';

export interface UserAsset {
  key: string;
  blob: Blob;
  url?: string;
}

export const getDeckBackgroundImage = async (): Promise<UserAsset | undefined> => {
  return getDeckImage();
};

export const getSlidesLocalImages = async ({deck}: {deck: Deck}): Promise<UserAsset[]> => {
  return getAssets<UserAsset>({deck, assets: getSlideLocalImages});
};

export const getSlidesLocalCharts = async ({deck}: {deck: Deck}): Promise<UserAsset[]> => {
  return getAssets<UserAsset>({deck, assets: getSlideLocalCharts});
};

export const getSlidesOnlineImages = async ({deck}: {deck: Deck}): Promise<UserAsset[]> => {
  return getAssets<UserAsset>({deck, assets: getSlideOnlineImages});
};

export const getSlidesOnlineCharts = async ({deck}: {deck: Deck}): Promise<UserAsset[]> => {
  return getAssets<UserAsset>({deck, assets: getSlideOnlineCharts});
};

const getAssets = async <T>({
  deck,
  assets
}: {
  deck: Deck;
  assets: ({slideId}: {slideId: string}) => Promise<T[] | undefined>;
}): Promise<T[]> => {
  if (!deck.data.slides || deck.data.slides.length <= 0) {
    return [];
  }

  try {
    const data: (T[] | undefined)[] = await Promise.all(deck.data.slides.map((slideId: string) => assets({slideId})));
    return [].concat(...data.filter((files: T[] | undefined) => files?.length));
  } catch (err) {
    throw new Error('Error while getting slides images');
  }
};

const getDeckImage = async (): Promise<UserAsset | undefined> => {
  const backgroundElement: HTMLElement = document.querySelector(`${deckSelector} > *[slot="background"]`);

  if (!backgroundElement) {
    return undefined;
  }

  const img: HTMLDeckgoLazyImgElement = backgroundElement.querySelector(SlotType.IMG);

  if (!img) {
    return undefined;
  }

  const {imgSrc} = img;

  if (!isLocalImage(img)) {
    return getUserAsset({url: imgSrc, type: 'images'});
  }

  return {
    key: imgSrc,
    blob: await get(imgSrc)
  };
};

const getSlideLocalImages = async ({slideId}: {slideId: string}): Promise<UserAsset[] | undefined> => {
  const imgs: HTMLDeckgoLazyImgElement[] | undefined = getSlideImages({slideId});

  if (!imgs || imgs.length <= 0) {
    return undefined;
  }

  // Filter online images (http...)
  const list: HTMLDeckgoLazyImgElement[] = Array.from(imgs).filter((img: HTMLDeckgoLazyImgElement) => {
    return isLocalImage(img);
  });

  const files: File[] = await getMany<File>(list.map(({imgSrc}: HTMLDeckgoLazyImgElement) => imgSrc));

  return files.map((blob: File) => ({
    key: `/assets/local/images/${blob.name}`,
    blob,
    type: 'local'
  }));
};

const getSlideOnlineImages = async ({slideId}: {slideId: string}): Promise<UserAsset[] | undefined> => {
  const imgs: HTMLDeckgoLazyImgElement[] | undefined = getSlideImages({slideId});

  if (!imgs || imgs.length <= 0) {
    return undefined;
  }

  // Filter online images (http...)
  const list: HTMLDeckgoLazyImgElement[] = Array.from(imgs).filter((img: HTMLDeckgoLazyImgElement) => {
    return !isLocalImage(img);
  });

  const promises: Promise<UserAsset | undefined>[] = list.map(({imgSrc}: HTMLDeckgoLazyImgElement) =>
    getUserAsset({url: imgSrc, type: 'images'})
  );

  return (await Promise.all(promises)).filter((asset: UserAsset | undefined) => asset !== undefined);
};

const getSlideImages = ({slideId}: {slideId: string}): HTMLDeckgoLazyImgElement[] | undefined => {
  const slideElement: HTMLElement = document.querySelector(`${deckSelector} > *[slide_id="${slideId}"]`);

  if (!slideElement) {
    return undefined;
  }

  const imgs: NodeListOf<HTMLDeckgoLazyImgElement> = slideElement.querySelectorAll(SlotType.IMG);

  if (!imgs || imgs.length <= 0) {
    return undefined;
  }

  // Filter deck background (which are cloned from the deck to the slides)
  return Array.from(imgs).filter((img: HTMLDeckgoLazyImgElement) => {
    return !(img.parentElement?.getAttribute('slot') === 'background' && !slideElement.hasAttribute('custom-background'));
  });
};

const isLocalImage = ({imgSrc}: HTMLDeckgoLazyImgElement): boolean => imgSrc !== undefined && imgSrc !== '' && !imgSrc.startsWith('http');

const getSlideLocalCharts = async ({slideId}: {slideId: string}): Promise<UserAsset[] | undefined> => {
  const src: string = getChartSrc({slideId});

  if (!src || src === undefined || src === '') {
    return undefined;
  }

  return [
    {
      key: src,
      blob: await get(src)
    }
  ];
};

const getSlideOnlineCharts = async ({slideId}: {slideId: string}): Promise<UserAsset[] | undefined> => {
  const src: string = getChartSrc({slideId});

  if (!src || src === undefined || src === '') {
    return undefined;
  }

  const asset: UserAsset = await getUserAsset({url: src, type: 'data'});
  return asset ? [asset] : undefined;
};

const getChartSrc = ({slideId}: {slideId: string}): string | undefined => {
  const slideElement: HTMLElement = document.querySelector(`${deckSelector} > *[slide_id="${slideId}"]`);

  if (!slideElement) {
    return undefined;
  }

  if (slideElement.tagName && slideElement.tagName.toUpperCase() !== 'deckgo-slide-chart'.toUpperCase()) {
    return undefined;
  }

  return (slideElement as HTMLDeckgoSlideChartElement).src;
};

const getUserAsset = async ({url, type}: {url: string; type: 'images' | 'data'}): Promise<UserAsset | undefined> => {
  try {
    const response: Response = await fetch(url);

    const blob: Blob = await response.blob();

    return {
      url,
      key: `/assets/online/${type}/${uuid()}`,
      blob
    };
  } catch (err) {
    // We ignore it, it remains referenced with its https link in the content.
    // For example: Tenor (Gif) does not support CORS fetch.
  }

  return undefined;
};
