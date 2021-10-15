import {get, set} from 'idb-keyval';

import {Deck, deckSelector, Slide, StorageFile} from '@deckdeckgo/editor';

import {SyncWindowData} from '../types/sync.window';

import {updateDeckBackground, updateSlideChart, updateSlideImages} from './sync.attributes.utils';

export const syncDeckBackground = async (data: SyncWindowData): Promise<void> => {
  // 1. We update the deck in the DOM
  updateDeckDOM(data);

  // 2. We replicate the same changes to the slides in the DOM
  await updateSlidesDOM(data);

  // 3. We update the indexedDB stored deck with the new downloadUrl.
  await updateDeckIDB(data);
};

export const syncSlideImage = async (data: SyncWindowData): Promise<void> => {
  const {slideId} = data;

  if (!slideId) {
    return;
  }

  // 1. We update the slide in the DOM
  await updateSlideImagesDOM(data);

  // 2. We update the indexedDB stored slide with the new downloadUrl.
  await updateSlideImagesIDB(data);
};

export const syncSlideChart = async (data: SyncWindowData): Promise<void> => {
  const {slideId} = data;

  if (!slideId) {
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

const updateSlideImagesDOM = async ({slideId, storageFile}: SyncWindowData) => {
  const slideElement: HTMLElement | null = document.querySelector(`${deckSelector} > *[slide_id="${slideId}"]`);

  const images: NodeListOf<HTMLDeckgoLazyImgElement> = slideElement?.querySelectorAll('deckgo-lazy-img');

  await updateImagesDOM({images, storageFile});
};

const updateSlideChartDOM = ({slideId, storageFile}: SyncWindowData) => {
  const slideElement: HTMLDeckgoSlideChartElement | null = document.querySelector(`${deckSelector} > *[slide_id="${slideId}"]`);

  if (!slideElement || !slideElement.nodeName || slideElement.nodeName.toLowerCase() !== 'deckgo-slide-chart') {
    return;
  }

  const {downloadUrl} = storageFile;

  slideElement.src = downloadUrl;
};

const updateSlidesDOM = async ({storageFile}: SyncWindowData) => {
  const images: NodeListOf<HTMLDeckgoLazyImgElement> = document.querySelectorAll(
    `${deckSelector} .deckgo-slide-container:not([custom-background]) *[slot="background"] deckgo-lazy-img`
  );

  await updateImagesDOM({images, storageFile});
};

const updateImagesDOM = async ({storageFile, images}: {images: NodeListOf<HTMLDeckgoLazyImgElement>; storageFile: StorageFile}) => {
  if (!images || images.length <= 0) {
    return;
  }

  const updateImage = async (img: HTMLDeckgoLazyImgElement) => {
    const {downloadUrl, name} = storageFile;

    img.imgSrc = downloadUrl;
    img.imgAlt = name;
  };

  const promises: Promise<void>[] = Array.from(images).map((img: HTMLDeckgoLazyImgElement) => updateImage(img));

  await Promise.all(promises);
};

const updateDeckIDB = async ({storageFile, src, deckId}: SyncWindowData) => {
  const deck: Deck = await get(`/decks/${deckId}`);

  if (!deck) {
    throw new Error('Deck not found and that is really weird here.');
  }

  const updateDeck: Deck = updateDeckBackground({deck, imgSrc: src, storageFile});

  await set(`/decks/${deck.id}`, updateDeck);
};

const updateSlideImagesIDB = async ({storageFile, src, slideId, deckId}: SyncWindowData) => {
  const slide: Slide = await getSlide({deckId, slideId});

  const updateSlide: Slide = updateSlideImages({
    slide,
    images: [
      {
        src,
        storageFile
      }
    ]
  });

  await setSlide({deckId, slideId, slide: updateSlide});
};

const updateSlideChartIDB = async ({storageFile, src, slideId, deckId}: SyncWindowData) => {
  const slide: Slide = await getSlide({deckId, slideId});

  const updateSlide: Slide = updateSlideChart({
    slide,
    chart: {
      src,
      storageFile
    }
  });

  await setSlide({deckId, slideId, slide: updateSlide});
};

const getSlide = async ({deckId, slideId}: {deckId: string; slideId: string}): Promise<Slide> => {
  const slide: Slide = await get(`/decks/${deckId}/slides/${slideId}`);

  if (!slide) {
    throw new Error('Slide not found and that is really weird here.');
  }

  return slide;
};

const setSlide = async ({deckId, slideId, slide}: {deckId: string; slideId: string; slide: Slide}) =>
  set(`/decks/${deckId}/slides/${slideId}`, slide);
