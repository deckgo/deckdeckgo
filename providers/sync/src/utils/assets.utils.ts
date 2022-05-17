import {Deck, deckSelector, Doc, docSelector, UserAsset} from '@deckdeckgo/editor';
import {get} from 'idb-keyval';
import {nanoid} from 'nanoid';

export const getDeckBackgroundImage = async (): Promise<UserAsset | undefined> => {
  return getDeckImage();
};

export const getSlidesLocalImages = async ({deck}: {deck: Deck | undefined}): Promise<UserAsset[]> => {
  return getAssets<UserAsset>({
    elementIds: deck?.data?.slides,
    assets: getLocalImages,
    selector: slideSelector
  });
};

export const getSlidesOnlineImages = async ({deck}: {deck: Deck | undefined}): Promise<UserAsset[]> => {
  return getAssets<UserAsset>({elementIds: deck?.data?.slides, assets: getSlideOnlineImages, selector: slideSelector});
};

export const getSlidesLocalCharts = async ({deck}: {deck: Deck | undefined}): Promise<UserAsset[]> => {
  return getAssets<UserAsset>({elementIds: deck?.data?.slides, assets: getSlideLocalCharts, selector: slideSelector});
};

export const getSlidesOnlineCharts = async ({deck}: {deck: Deck | undefined}): Promise<UserAsset[]> => {
  return getAssets<UserAsset>({elementIds: deck?.data?.slides, assets: getSlideOnlineCharts, selector: slideSelector});
};

export const getParagraphsLocalImages = async ({doc}: {doc: Doc | undefined}): Promise<UserAsset[]> => {
  return getAssets<UserAsset>({
    elementIds: doc?.data?.paragraphs,
    assets: getLocalImages,
    selector: paragraphSelector
  });
};

export const getParagraphsOnlineImages = async ({doc}: {doc: Doc | undefined}): Promise<UserAsset[]> => {
  return getAssets<UserAsset>({elementIds: doc?.data?.paragraphs, assets: getSlideOnlineImages, selector: paragraphSelector});
};

const slideSelector = (id: string) => {
  return {selector: `${deckSelector} > *[slide_id="${id}"]`};
};

const paragraphSelector = (id: string) => {
  return {selector: `${docSelector} > article *[paragraph_id="${id}"]`};
};

const getAssets = async <T>({
  elementIds,
  selector,
  assets
}: {
  elementIds: string[] | undefined;
  selector: (id: string) => {selector: string};
  assets: ({selector}: {selector: string}) => Promise<T[] | undefined>;
}): Promise<T[]> => {
  if (!elementIds || elementIds.length <= 0) {
    return [];
  }

  try {
    const data: (T[] | undefined)[] = await Promise.all(elementIds.map((id: string) => assets(selector(id))));
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

  const img: HTMLDeckgoLazyImgElement = backgroundElement.querySelector('deckgo-lazy-img');

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

const getLocalImages = async ({selector}: {selector: string}): Promise<UserAsset[] | undefined> => {
  const imgs: HTMLDeckgoLazyImgElement[] | undefined = getImages({selector});

  if (!imgs || imgs.length <= 0) {
    return undefined;
  }

  // Filter online images (http...)
  const list: HTMLDeckgoLazyImgElement[] = Array.from(imgs).filter((img: HTMLDeckgoLazyImgElement) => {
    return isLocalImage(img);
  });

  // Read files from idb - preserve keys (the file name might not match the key since we `encodeFilename` with offline providers when we upload the images)
  const files: {key: string; blob: File}[] = await Promise.all(
    list.map(async ({imgSrc}: HTMLDeckgoLazyImgElement) => {
      const blob: File = await get<File>(imgSrc);

      return {
        key: imgSrc,
        blob
      };
    })
  );

  return files.map(({key, blob}: {key: string; blob: File}) => ({
    key,
    blob,
    type: 'local'
  }));
};

const getSlideOnlineImages = async ({selector}: {selector: string}): Promise<UserAsset[] | undefined> => {
  const imgs: HTMLDeckgoLazyImgElement[] | undefined = getImages({selector});

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

const getImages = ({selector}: {selector: string}): HTMLDeckgoLazyImgElement[] | undefined => {
  const element: HTMLElement = document.querySelector(selector);

  if (!element) {
    return undefined;
  }

  const imgs: NodeListOf<HTMLDeckgoLazyImgElement> = element.querySelectorAll('deckgo-lazy-img');

  if (!imgs || imgs.length <= 0) {
    return undefined;
  }

  // Filter deck background (which are cloned from the deck to the slides)
  return Array.from(imgs).filter((img: HTMLDeckgoLazyImgElement) => {
    return !(img.parentElement?.getAttribute('slot') === 'background' && !element.hasAttribute('custom-background'));
  });
};

const isLocalImage = ({imgSrc}: HTMLDeckgoLazyImgElement): boolean => imgSrc !== undefined && imgSrc !== '' && !imgSrc.startsWith('http');

const getSlideLocalCharts = async ({selector}: {selector: string}): Promise<UserAsset[] | undefined> => {
  const src: string = getChartSrc({selector});

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

const getSlideOnlineCharts = async ({selector}: {selector: string}): Promise<UserAsset[] | undefined> => {
  const src: string = getChartSrc({selector});

  if (!src || src === undefined || src === '') {
    return undefined;
  }

  const asset: UserAsset = await getUserAsset({url: src, type: 'data'});
  return asset ? [asset] : undefined;
};

const getChartSrc = ({selector}: {selector: string}): string | null => {
  const slideElement: HTMLElement = document.querySelector(selector);

  if (!slideElement) {
    return null;
  }

  if (slideElement.tagName && slideElement.tagName.toUpperCase() !== 'deckgo-slide-chart'.toUpperCase()) {
    return null;
  }

  return slideElement.getAttribute('src');
};

const getUserAsset = async ({url, type}: {url: string; type: 'images' | 'data'}): Promise<UserAsset | undefined> => {
  try {
    const response: Response = await fetch(url);

    const blob: Blob = await response.blob();

    return {
      url,
      key: `/assets/online/${type}/${nanoid()}`,
      blob
    };
  } catch (err) {
    // We ignore it, it remains referenced with its https link in the content.
    // For example: Tenor (Gif) does not support CORS fetch.
  }

  return undefined;
};
