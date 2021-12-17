import {get, set} from 'idb-keyval';

import {Deck, deckSelector, Paragraph, Slide, StorageFile} from '@deckdeckgo/editor';

import {SyncWindowData} from '../types/sync.window';

import {updateDeckBackground, updateParagraphImages, updateSlideChart, updateSlideImages} from './sync.attributes.utils';

export const syncDeckBackground = async (data: SyncWindowData): Promise<void> => {
  // 1. We update the deck in the DOM
  updateDeckDOM(data);

  // 2. We replicate the same changes to the slides in the DOM
  await updateSlidesDOM(data);

  // 3. We update the indexedDB stored deck with the new downloadUrl.
  await updateDeckIDB(data);
};

export const syncSlideImage = async (data: SyncWindowData): Promise<void> => {
  const {selector} = data;

  if (!selector) {
    return;
  }

  // 1. We update the slide in the DOM
  await updateElementImagesDOM(data);

  // 2. We update the indexedDB stored slide with the new downloadUrl.
  await updateSlideImagesIDB(data);
};

export const syncParagraphImage = async (data: SyncWindowData): Promise<void> => {
  const {selector} = data;

  if (!selector) {
    return;
  }

  // 1. We update the paragraph in the DOM
  await updateElementImagesDOM(data);

  // 2. We update the indexedDB stored paragraph with the new downloadUrl.
  await updateParagraphImagesIDB(data);
};

export const syncSlideChart = async (data: SyncWindowData): Promise<void> => {
  const {selector} = data;

  if (!selector) {
    return;
  }

  // 1. We update the slide in the DOM
  updateSlideChartDOM(data);

  // 2. We update the indexedDB stored slide with the new downloadUrl.
  await updateSlideChartIDB(data);
};

const updateDeckDOM = ({storageFile}: SyncWindowData) => {
  const backgroundElement: HTMLElement | null = document.querySelector(`${deckSelector} > *[slot="background"]`);

  const img: HTMLDeckgoLazyImgElement | null = backgroundElement?.querySelector('deckgo-lazy-img');

  if (!img) {
    return;
  }

  const {downloadUrl, name} = storageFile;

  img.imgSrc = downloadUrl;
  img.imgAlt = name;
};

const updateElementImagesDOM = async ({selector, storageFile, src}: SyncWindowData) => {
  const element: HTMLElement | null = document.querySelector(selector);

  // The paragraph might be an image itself
  if (element?.nodeName.toLowerCase() === 'deckgo-lazy-img') {
    await updateImagesDOM({images: [element as HTMLDeckgoLazyImgElement], storageFile, src});
    return;
  }

  const images: NodeListOf<HTMLDeckgoLazyImgElement> = element?.querySelectorAll('deckgo-lazy-img');

  await updateImagesDOM({images: Array.from(images), storageFile, src});
};

const updateSlideChartDOM = ({selector, storageFile}: SyncWindowData) => {
  const slideElement: HTMLDeckgoSlideChartElement | null = document.querySelector(selector);

  if (!slideElement || !slideElement.nodeName || slideElement.nodeName.toLowerCase() !== 'deckgo-slide-chart') {
    return;
  }

  const {downloadUrl} = storageFile;

  slideElement.src = downloadUrl;
};

const updateSlidesDOM = async ({storageFile, src}: SyncWindowData) => {
  const images: NodeListOf<HTMLDeckgoLazyImgElement> = document.querySelectorAll(
    `${deckSelector} .deckgo-slide-container:not([custom-background]) *[slot="background"] deckgo-lazy-img`
  );

  await updateImagesDOM({images: Array.from(images), storageFile, src});
};

const updateImagesDOM = async ({storageFile, images, src}: {images: HTMLDeckgoLazyImgElement[]; storageFile: StorageFile; src: string}) => {
  const matchingImages: HTMLDeckgoLazyImgElement[] = images.filter((img: HTMLDeckgoLazyImgElement) => img.imgSrc === src);

  if (!matchingImages || matchingImages.length <= 0) {
    return;
  }

  const updateImage = async (img: HTMLDeckgoLazyImgElement) => {
    const {downloadUrl, name} = storageFile;

    img.imgSrc = downloadUrl;
    img.imgAlt = name;
  };

  const promises: Promise<void>[] = Array.from(matchingImages).map((img: HTMLDeckgoLazyImgElement) => updateImage(img));

  await Promise.all(promises);
};

const updateDeckIDB = async ({storageFile, src, key}: SyncWindowData) => {
  const deck: Deck = await getData<Deck>({key});

  const updateDeck: Deck = updateDeckBackground({deck, imgSrc: src, storageFile});

  await setData<Deck>({key, data: updateDeck});
};

const updateSlideImagesIDB = async ({storageFile, src, key}: SyncWindowData) => {
  const slide: Slide = await getData<Slide>({key});

  const updateSlide: Slide = updateSlideImages({
    slide,
    images: [
      {
        src,
        storageFile
      }
    ]
  });

  await setData<Slide>({key, data: updateSlide});
};

const updateParagraphImagesIDB = async ({storageFile, src, key}: SyncWindowData) => {
  const paragraph: Paragraph = await getData<Paragraph>({key});

  const updateParagraph: Paragraph = updateParagraphImages({
    paragraph,
    images: [
      {
        src,
        storageFile
      }
    ]
  });

  await setData<Paragraph>({key, data: updateParagraph});
};

const updateSlideChartIDB = async ({storageFile, src, key}: SyncWindowData) => {
  const slide: Slide = await getData<Slide>({key});

  const updateSlide: Slide = updateSlideChart({
    slide,
    chart: {
      src,
      storageFile
    }
  });

  await setData<Slide>({key, data: updateSlide});
};

const getData = async <T>({key}: {key: string}): Promise<T> => {
  const data: T = await get(key);

  if (!data) {
    throw new Error('Data not found and that is really weird here.');
  }

  return data;
};

const setData = async <T>({key, data}: {key: string; data: T}) => set(key, data);
