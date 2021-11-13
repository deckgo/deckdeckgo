import {Identity} from '@dfinity/agent';

import {del, get} from 'idb-keyval';

import {Deck, deckSelector, docSelector, Paragraph, Slide, SlideTemplate, StorageFile} from '@deckdeckgo/editor';

import {SyncWindow, SyncWindowEventMsg} from '../types/sync.window';
import {SyncStorage, SyncStorageSlide} from '../types/sync.storage';

import {uploadFileIC} from '../providers/storage/storage.ic';

const imagesRegex: RegExp = /((<deckgo-lazy-img.*?)(img-src=)(.*?")(.*?[^"]*)(.*?"))/g;

export const uploadDeckBackgroundAssets = async ({
  deck,
  identity,
  host,
  syncWindow
}: {
  deck: Deck;
  identity: Identity;
  host: string;
  syncWindow: SyncWindow;
}): Promise<SyncStorage> => {
  const {background} = deck.data;

  if (!background) {
    return {
      src: undefined,
      storageFile: undefined
    };
  }

  const results: string[][] = [...background.matchAll(imagesRegex)];

  // Only one image in the background is currently supported
  if (results?.length !== 1) {
    return {
      src: undefined,
      storageFile: undefined
    };
  }

  const imgSrc: string | undefined = results[0][5];

  return uploadData({
    src: imgSrc,
    key: `/decks/${deck.id}`,
    identity,
    host,
    syncWindow,
    msg: 'deckdeckgo_sync_deck_background',
    folder: 'images'
  });
};

export const uploadSlideAssets = async ({
  deckId,
  slide,
  identity,
  host,
  syncWindow
}: {
  deckId: string;
  slide: Slide;
  identity: Identity;
  host: string;
  syncWindow: SyncWindow;
}): Promise<SyncStorageSlide> => {
  const images: SyncStorage[] | undefined = await uploadSlideImages({
    deckId,
    slide,
    identity,
    host,
    syncWindow
  });
  const chart: SyncStorage | undefined = await uploadSlideChart({deckId, slide, identity, host, syncWindow});

  return {
    images,
    chart
  };
};

const uploadSlideImages = async ({
  deckId,
  slide,
  identity,
  host,
  syncWindow
}: {
  deckId: string;
  slide: Slide;
  identity: Identity;
  host: string;
  syncWindow: SyncWindow;
}): Promise<SyncStorage[] | undefined> => {
  const {content} = slide.data;

  if (!content) {
    return undefined;
  }

  const results: string[][] = [...content.matchAll(imagesRegex)];

  const promises: Promise<SyncStorage>[] | undefined = results?.map((result: string[]) => {
    const imgSrc: string = result[5];

    return uploadData({
      src: imgSrc,
      key: `/decks/${deckId}/slides/${slide.id}`,
      selector: `${deckSelector} > *[slide_id="${slide.id}"]`,
      identity,
      host,
      syncWindow,
      msg: 'deckdeckgo_sync_slide_image',
      folder: 'images'
    });
  });

  return Promise.all(promises);
};

const uploadSlideChart = async ({
  deckId,
  slide,
  identity,
  host,
  syncWindow
}: {
  deckId: string;
  slide: Slide;
  identity: Identity;
  host: string;
  syncWindow: SyncWindow;
}): Promise<SyncStorage | undefined> => {
  const {content, template, attributes} = slide.data;

  if (!content || template !== SlideTemplate.CHART || !attributes) {
    return undefined;
  }

  const {src} = attributes;

  return uploadData({
    src,
    key: `/decks/${deckId}/slides/${slide.id}`,
    selector: `${deckSelector} > *[slide_id="${slide.id}"]`,
    identity,
    host,
    syncWindow,
    msg: 'deckdeckgo_sync_slide_chart',
    folder: 'data'
  });
};

const uploadData = ({
  src,
  key,
  selector,
  host,
  identity,
  syncWindow,
  msg,
  folder
}: {
  src: string | undefined;
  key: string;
  selector?: string;
  host: string;
  identity: Identity;
  syncWindow: SyncWindow;
  msg: SyncWindowEventMsg;
  folder: 'images' | 'data';
}): Promise<SyncStorage> => {
  return new Promise<SyncStorage>(async (resolve, reject) => {
    try {
      if (!src || src === '' || /^https?:\/\/.+\/.+$/.test(src)) {
        resolve({
          src: undefined,
          storageFile: undefined
        });
        return;
      }

      const data: File = await get(src);

      if (!data) {
        // We didn't the corresponding image. Instead of crashing an error we go through, user will notice that nothing is displayed.
        // Better than blocking the all process and reaching an intermediate state.
        resolve({
          src: undefined,
          storageFile: undefined
        });
        return;
      }

      // 1. We upload the file to the storage cloud
      const storageFile: StorageFile | undefined = await uploadFileIC({
        data,
        folder,
        maxSize: 10485760,
        identity,
        host
      });

      if (!storageFile) {
        reject(`Data ${src} upload has failed.`);
        return;
      }

      // 2. We update the DOM and IDB (currently saved data)
      await syncWindow({
        msg,
        data: {
          src,
          key,
          selector,
          storageFile
        }
      });

      // 3. All good, we don't need the image in the indexedDB anymore
      await del(src);

      resolve({
        src,
        storageFile
      });
    } catch (err) {
      reject(err);
    }
  });
};

export const uploadParagraphImages = async ({
  docId,
  paragraph,
  identity,
  host,
  syncWindow
}: {
  docId: string;
  paragraph: Paragraph;
  identity: Identity;
  host: string;
  syncWindow: SyncWindow;
}): Promise<SyncStorage[] | undefined> => {
  const {children} = paragraph.data;

  if (!children || children.length <= 0) {
    return undefined;
  }

  const content: string = children.join('');

  const results: string[][] = [...content.matchAll(imagesRegex)];

  const promises: Promise<SyncStorage>[] | undefined = results?.map((result: string[]) => {
    const imgSrc: string = result[5];

    return uploadData({
      src: imgSrc,
      key: `/docs/${docId}/paragraphs/${paragraph.id}`,
      selector: `${docSelector} > article *[paragraph_id="${paragraph.id}"]`,
      identity,
      host,
      syncWindow,
      msg: 'deckdeckgo_sync_paragraph_image',
      folder: 'images'
    });
  });

  return Promise.all(promises);
};
