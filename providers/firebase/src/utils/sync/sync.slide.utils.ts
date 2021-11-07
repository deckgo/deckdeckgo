import firebase from 'firebase/app';

import {get} from 'idb-keyval';

import {deckSelector, Slide, SlideAttributes, SyncDataSlide} from '@deckdeckgo/editor';

import {prepareAttributes} from '../data/firestore.utils';

import {deleteSlide, updateSlide} from '../../providers/data/slide.firebase';
import {uploadLocalCharts, uploadLocalImages} from './sync.storage.utils';

export const uploadSlides = ({data, userId}: {data: SyncDataSlide[] | undefined; userId: string}): Promise<void> => {
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
      await uploadLocalCharts<Slide>({element: slideElement, key: `/decks/${deckId}/slides/${slideId}`, userId});

      const updateContent = ({data: slide, imgSrc, downloadUrl}: {data: Slide; imgSrc: string; downloadUrl: string}): Slide => {
        if (!slide.data.content) {
          return slide;
        }

        slide.data.content = slide.data.content.replace(`img-src="${imgSrc}"`, `img-src="${downloadUrl}"`);
        slide.data.content = slide.data.content.replace(`img-alt="${imgSrc}"`, `img-alt="${downloadUrl}"`);

        return slide;
      };

      await uploadLocalImages({element: slideElement, key: `/decks/${deckId}/slides/${slideId}`, userId, updateContent});

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

      slide.data.attributes = prepareAttributes<SlideAttributes>(slide.data.attributes);

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

export const deleteSlides = async (data: SyncDataSlide[] | undefined): Promise<void> => {
  if (!data || data.length <= 0) {
    return;
  }

  const promises: Promise<void>[] = data.map(({deckId, slideId}: SyncDataSlide) => deleteSlide(deckId, slideId));
  await Promise.all(promises);
};
