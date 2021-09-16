import firebase from 'firebase/app';

import {del, get, set} from 'idb-keyval';

import {
  Deck,
  DeckAttributes,
  deckSelector,
  Slide,
  SlideAttributes,
  StorageFile,
  SyncData,
  SyncDataDeck,
  SyncDataSlide
} from '@deckdeckgo/editor';

import {deleteSlide, updateSlide} from '../data/slide.firebase';
import {updateDeck} from '../data/deck.firebase';

import {uploadFile} from '../storage/storage.firebase';

import {FirestoreUtils} from '../../utils/firestore.utils';

export const sync = async ({
  syncData,
  userId,
  clean
}: {
  syncData: SyncData | undefined;
  userId: string;
  clean: ({syncedAt}: SyncData) => Promise<void>;
}) => {
  if (!syncData) {
    return;
  }

  // TODO: when we will solve the storage question, we can leverage the data provided as parameter instead of querying idb here again

  const {updateDecks, updateSlides, deleteSlides: deleteSlidesData} = syncData;

  // First decks because it contains information for the permission and the list of slides
  await uploadDecks({data: updateDecks, userId});

  await uploadSlides({data: updateSlides, userId});

  await deleteSlides(deleteSlidesData);

  // TODO: handle delete decks here?

  await clean(syncData);
};

const uploadSlides = ({data, userId}: {data: SyncDataSlide[] | undefined; userId: string}): Promise<void> => {
  return new Promise<void>(async (resolve, reject) => {
    if (!data || data.length <= 0) {
      resolve();
      return;
    }

    try {
      const promises: Promise<void>[] = data.map(({deckId, slideId}: SyncDataSlide) => uploadSlide({deckId, slideId, userId}));

      await Promise.all(promises);

      resolve();
    } catch (err) {
      reject(err);
    }
  });
};

const uploadSlide = async ({deckId, slideId, userId}: {deckId: string; slideId: string; userId: string}): Promise<void> => {
  await uploadSlideLocalUserAssets({deckId, slideId, userId});
  await uploadSlideData(deckId, slideId);
};

const uploadSlideLocalUserAssets = ({deckId, slideId, userId}: {deckId: string; slideId: string; userId: string}): Promise<void> => {
  return new Promise<void>(async (resolve, reject) => {
    const slideElement: HTMLElement = document.querySelector(`${deckSelector} > *[slide_id="${slideId}"]`);

    if (!slideElement) {
      resolve();
      return;
    }

    try {
      await uploadSlideLocalCharts({slideElement, deckId, slideId, userId});
      await uploadSlideLocalImages({slideElement, deckId, slideId, userId});

      resolve();
    } catch (err) {
      reject(err);
    }
  });
};

const uploadSlideLocalCharts = ({
  slideElement,
  deckId,
  slideId,
  userId
}: {
  slideElement: HTMLElement;
  deckId: string;
  slideId: string;
  userId: string;
}): Promise<void> => {
  return new Promise<void>(async (resolve, reject) => {
    try {
      if (slideElement.tagName && slideElement.tagName.toUpperCase() !== 'deckgo-slide-chart'.toUpperCase()) {
        resolve();
        return;
      }

      const src: string = (slideElement as HTMLDeckgoSlideChartElement).src;

      if (!src || src === undefined || src === '') {
        resolve();
        return;
      }

      const data: File = await get(src);

      if (!data) {
        // We didn't the corresponding image. Instead of crashing an error we go through, user will notice that nothing is displayed.
        // Better than blocking the all process and reaching an intermediate state.
        resolve();
        return;
      }

      // 1. We upload the file to the storage cloud
      const storageFile: StorageFile | undefined = await uploadFile({
        data,
        folder: 'data',
        maxSize: 10485760,
        userId
      });

      if (!storageFile) {
        reject(`Chart ${src} upload has failed.`);
        return;
      }

      // 2. We update the indexedDB stored slide with the new downloadUrl. This stored slide will be later updated back to the database.
      const slide: Slide = await get(`/decks/${deckId}/slides/${slideId}`);

      if (!slide) {
        reject('Slide not found and that is really weird here.');
        return;
      }

      slide.data.attributes.src = storageFile.downloadUrl;

      await set(`/decks/${deckId}/slides/${slideId}`, slide);

      // 3. We update the DOM
      (slideElement as HTMLDeckgoSlideChartElement).src = storageFile.downloadUrl;

      // 4. All good, we don't need the image in the indexedDB anymore
      await del(src);

      resolve();
    } catch (err) {
      reject(err);
    }
  });
};

const uploadSlideLocalImages = ({
  slideElement,
  deckId,
  slideId,
  userId
}: {
  slideElement: HTMLElement;
  deckId: string;
  slideId: string;
  userId: string;
}): Promise<void> => {
  return new Promise<void>(async (resolve, reject) => {
    try {
      const imgs: NodeListOf<HTMLDeckgoLazyImgElement> = slideElement.querySelectorAll('deckgo-lazy-img');

      if (!imgs || imgs.length <= 0) {
        resolve();
        return;
      }

      // Filter online images (http...) and deck background (which are cloned from the deck to the slides)
      const list: HTMLDeckgoLazyImgElement[] = Array.from(imgs).filter((img: HTMLDeckgoLazyImgElement) => {
        return (
          img.imgSrc !== undefined &&
          img.imgSrc !== '' &&
          img.imgSrc.indexOf('http') === -1 &&
          !(img.parentElement && img.parentElement.getAttribute('slot') === 'background' && !slideElement.hasAttribute('custom-background'))
        );
      });

      if (!list || list.length <= 0) {
        resolve();
        return;
      }

      const promises: Promise<void>[] = list.map((img: HTMLDeckgoLazyImgElement) => {
        return uploadSlideLocalImage({img, deckId, slideId, userId});
      });

      await Promise.all(promises);

      resolve();
    } catch (err) {
      reject(err);
    }
  });
};

const uploadSlideLocalImage = ({
  img,
  deckId,
  slideId,
  userId
}: {
  img: HTMLDeckgoLazyImgElement;
  deckId: string;
  slideId: string;
  userId: string;
}): Promise<void> => {
  return new Promise<void>(async (resolve, reject) => {
    try {
      const data: File = await get(img.imgSrc);

      if (!data) {
        // We didn't the corresponding image. Instead of crashing an error we go through, user will notice that nothing is displayed.
        // Better than blocking the all process and reaching an intermediate state.
        resolve();
        return;
      }

      // 1. We upload the file to the storage cloud
      const storageFile: StorageFile | undefined = await uploadFile({
        data,
        folder: 'images',
        maxSize: 10485760,
        userId
      });

      if (!storageFile) {
        reject(`Image ${img.imgSrc} upload has failed.`);
        return;
      }

      // 2. We update the indexedDB stored slide with the new downloadUrl. This stored slide will be later uploaded back to the database.
      const slide: Slide = await get(`/decks/${deckId}/slides/${slideId}`);

      if (!slide) {
        reject('Slide not found and that is really weird here.');
        return;
      }

      slide.data.content = slide.data.content.replace(`img-src="${img.imgSrc}"`, `img-src="${storageFile.downloadUrl}"`);
      slide.data.content = slide.data.content.replace(`img-alt="${img.imgSrc}"`, `img-alt="${storageFile.downloadUrl}"`);

      await set(`/decks/${deckId}/slides/${slideId}`, slide);

      // 3. We update the DOM
      img.imgSrc = storageFile.downloadUrl;
      img.imgAlt = storageFile.downloadUrl;

      // 4. All good, we don't need the image in the indexedDB anymore
      await del(img.imgSrc);

      resolve();
    } catch (err) {
      reject(err);
    }
  });
};

const uploadSlideData = (deckId: string, slideId: string): Promise<void> => {
  return new Promise<void>(async (resolve, reject) => {
    try {
      const slide: Slide = await get(`/decks/${deckId}/slides/${slideId}`);

      if (!slide || !slide.data) {
        // If upload process end up in error in a previous try, some slides might have already been uploaded correctly and remove from the local db
        resolve();
        return;
      }

      slide.data.attributes = (await FirestoreUtils.prepareAttributes(slide.data.attributes)) as SlideAttributes;

      if (slide.data.content === null) {
        // @ts-ignore
        slide.data.content = firebase.firestore.FieldValue.delete();
      }

      await updateSlide(deckId, slide);

      resolve();
    } catch (err) {
      reject(err);
    }
  });
};

const deleteSlides = async (data: SyncDataSlide[] | undefined): Promise<void> => {
  if (!data || data.length <= 0) {
    return;
  }

  const promises: Promise<void>[] = data.map(({deckId, slideId}: SyncDataSlide) => deleteSlide(deckId, slideId));
  await Promise.all(promises);
};

const uploadDecks = async ({data, userId}: {data: SyncDataDeck[] | undefined; userId: string}): Promise<void> => {
  if (!data || data.length <= 0) {
    return;
  }

  const promises: Promise<void>[] = data.map((deck: SyncDataDeck) => uploadDeck({data: deck, userId}));
  await Promise.all(promises);
};

const uploadDeck = async ({data, userId}: {data: SyncDataDeck; userId: string}): Promise<void> => {
  const {deckId}: SyncDataDeck = data;

  await uploadDeckBackgroundAssets({deckId, userId});

  const deck: Deck = await get(`/decks/${deckId}`);

  if (!deck) {
    return;
  }

  await uploadDeckData({
    id: deckId,
    data: {
      ...deck.data,
      owner_id: userId
    }
  });
};

const uploadDeckData = (deck: Deck): Promise<Deck> => {
  return new Promise<Deck>(async (resolve, reject) => {
    try {
      if (!deck || !deck.data) {
        reject('Missing deck for upload');
        return;
      }

      deck.data.attributes = (await FirestoreUtils.prepareAttributes(deck.data.attributes)) as DeckAttributes;

      if (deck.data.background === null) {
        // @ts-ignore
        deck.data.background = firebase.firestore.FieldValue.delete();
      }

      if (deck.data.header === null) {
        // @ts-ignore
        deck.data.header = firebase.firestore.FieldValue.delete();
      }

      if (deck.data.footer === null) {
        // @ts-ignore
        deck.data.footer = firebase.firestore.FieldValue.delete();
      }

      const persistedDeck: Deck = await updateDeck(deck);

      resolve(persistedDeck);
    } catch (err) {
      reject(err);
    }
  });
};

const uploadDeckBackgroundAssets = ({deckId, userId}: {deckId: string; userId: string}): Promise<void> => {
  return new Promise<void>(async (resolve, reject) => {
    const backgroundElement: HTMLElement = document.querySelector(`${deckSelector} > *[slot="background"]`);

    if (!backgroundElement) {
      resolve();
      return;
    }

    try {
      const img: HTMLDeckgoLazyImgElement = backgroundElement.querySelector('deckgo-lazy-img');

      if (!img) {
        resolve();
        return;
      }

      await uploadDeckLocalImage({img, deckId, userId});

      resolve();
    } catch (err) {
      reject(err);
    }
  });
};

const uploadDeckLocalImage = ({img, deckId, userId}: {img: HTMLDeckgoLazyImgElement; deckId: string; userId: string}): Promise<void> => {
  return new Promise<void>(async (resolve, reject) => {
    try {
      const data: File = await get(img.imgSrc);

      if (!data) {
        // We didn't the corresponding image. Instead of crashing an error we go through, user will notice that nothing is displayed.
        // Better than blocking the all process and reaching an intermediate state.
        resolve();
        return;
      }

      // 1. We upload the file to the storage cloud
      const storageFile: StorageFile | undefined = await uploadFile({
        data,
        folder: 'images',
        maxSize: 10485760,
        userId
      });

      if (!storageFile) {
        reject(`Image ${img.imgSrc} upload has failed.`);
        return;
      }

      // 2. We update the indexedDB stored deck with the new downloadUrl. This stored deck will be later updated back to the database.
      const deck: Deck = await get(`/decks/${deckId}`);

      if (!deck) {
        reject('Deck not found and that is really weird here.');
        return;
      }

      deck.data.background = deck.data.background.replace(`img-src="${img.imgSrc}"`, `img-src="${storageFile.downloadUrl}"`);
      deck.data.background = deck.data.background.replace(`img-alt="${img.imgSrc}"`, `img-alt="${storageFile.downloadUrl}"`);

      await set(`/decks/${deck.id}`, deck);

      // 3. We update the DOM
      img.imgSrc = storageFile.downloadUrl;
      img.imgAlt = storageFile.downloadUrl;

      // 4. All good, we don't need the image in the indexedDB anymore
      await del(img.imgSrc);

      resolve();
    } catch (err) {
      reject(err);
    }
  });
};
