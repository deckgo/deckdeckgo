import {del, get, set} from 'idb-keyval';

import {StorageFile} from '@deckdeckgo/editor';

import {uploadFile} from '../../providers/storage/storage.firebase';

export const uploadLocalCharts = <T>({element, key, userId}: {element: HTMLElement; key: string; userId: string}): Promise<void> => {
  return new Promise<void>(async (resolve, reject) => {
    try {
      if (element.tagName && element.tagName.toUpperCase() !== 'deckgo-slide-chart'.toUpperCase()) {
        resolve();
        return;
      }

      const src: string = (element as HTMLDeckgoSlideChartElement).src;

      if (!src || src === undefined || src === '') {
        resolve();
        return;
      }

      const file: File = await get(src);

      if (!file) {
        // We didn't the corresponding image. Instead of crashing an error we go through, user will notice that nothing is displayed.
        // Better than blocking the all process and reaching an intermediate state.
        resolve();
        return;
      }

      // 1. We upload the file to the storage cloud
      const storageFile: StorageFile | undefined = await uploadFile({
        data: file,
        folder: 'data',
        maxSize: 10485760,
        userId
      });

      if (!storageFile) {
        reject(`Chart ${src} upload has failed.`);
        return;
      }

      // 2. We update the indexedDB stored slide or paragraph with the new downloadUrl. This stored slide or paragraph will be later updated back to the database.
      const data: T = await get(key);

      if (!data) {
        reject('Data not found and that is really weird here.');
        return;
      }

      data['data'].attributes.src = storageFile.downloadUrl;

      await set(key, data);

      // 3. We update the DOM
      (element as HTMLDeckgoSlideChartElement).src = storageFile.downloadUrl;

      // 4. All good, we don't need the image in the indexedDB anymore
      await del(src);

      resolve();
    } catch (err) {
      reject(err);
    }
  });
};

export const uploadLocalImages = <T>({
  element,
  key,
  userId,
  updateContent
}: {
  element: HTMLElement;
  key: string;
  userId: string;
  updateContent: ({data, imgSrc, downloadUrl}: {data: T; imgSrc: string; downloadUrl: string}) => T;
}): Promise<void> => {
  return new Promise<void>(async (resolve, reject) => {
    try {
      const imgs: NodeListOf<HTMLDeckgoLazyImgElement> = element.querySelectorAll('deckgo-lazy-img');

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
          !(img.parentElement && img.parentElement.getAttribute('slot') === 'background' && !element.hasAttribute('custom-background'))
        );
      });

      if (!list || list.length <= 0) {
        resolve();
        return;
      }

      const promises: Promise<void>[] = list.map((img: HTMLDeckgoLazyImgElement) => {
        return uploadLocalImage<T>({img, key, userId, updateContent});
      });

      await Promise.all(promises);

      resolve();
    } catch (err) {
      reject(err);
    }
  });
};

const uploadLocalImage = <T>({
  img,
  key,
  userId,
  updateContent
}: {
  img: HTMLDeckgoLazyImgElement;
  key: string;
  userId: string;
  updateContent: ({data, imgSrc, downloadUrl}: {data: T; imgSrc: string; downloadUrl: string}) => T;
}): Promise<void> => {
  return new Promise<void>(async (resolve, reject) => {
    try {
      const file: File = await get(img.imgSrc);

      if (!file) {
        // We didn't the corresponding image. Instead of crashing an error we go through, user will notice that nothing is displayed.
        // Better than blocking the all process and reaching an intermediate state.
        resolve();
        return;
      }

      // 1. We upload the file to the storage cloud
      const storageFile: StorageFile | undefined = await uploadFile({
        data: file,
        folder: 'images',
        maxSize: 10485760,
        userId
      });

      if (!storageFile) {
        reject(`Image ${img.imgSrc} upload has failed.`);
        return;
      }

      // 2. We update the indexedDB stored slide or paragraph with the new downloadUrl. This stored slide or paragraph will be later uploaded back to the database.
      const data: T = await get(key);

      if (!data) {
        reject('Data not found and that is really weird here.');
        return;
      }

      const updateData: T = updateContent({data, imgSrc: img.imgSrc, downloadUrl: storageFile.downloadUrl});

      await set(key, updateData);

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
