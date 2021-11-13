import {Deck, Paragraph, Slide, StorageFile} from '@deckdeckgo/editor';
import {SyncStorage} from '../types/sync.storage';

export const updateDeckBackground = ({
  deck,
  storageFile,
  imgSrc
}: {
  deck: Deck;
  storageFile: StorageFile | undefined;
  imgSrc: string | undefined;
}): Deck => {
  if (!storageFile || !imgSrc) {
    return {...deck};
  }

  return {
    id: deck.id,
    data: {
      ...deck.data,
      updated_at: new Date(),
      background: updateImgSrcAlt({data: deck.data.background, imgSrc, storageFile})
    }
  };
};

export const updateSlideImages = ({slide, images}: {slide: Slide; images: SyncStorage[] | undefined}): Slide => {
  if (!images) {
    return {...slide};
  }

  const validImages: {src: string; storageFile: StorageFile}[] = images.filter(
    ({src, storageFile}: SyncStorage) => src !== undefined && storageFile !== undefined
  );

  let {content} = slide.data;

  validImages.forEach(({src, storageFile}: SyncStorage) => {
    content = updateImgSrcAlt({data: content, storageFile, imgSrc: src});
  });

  return {
    id: slide.id,
    data: {
      ...slide.data,
      updated_at: new Date(),
      content
    }
  };
};

export const updateParagraphImages = ({paragraph, images}: {paragraph: Paragraph; images: SyncStorage[] | undefined}): Paragraph => {
  if (!images) {
    return {...paragraph};
  }

  const validImages: {src: string; storageFile: StorageFile}[] = images.filter(
    ({src, storageFile}: SyncStorage) => src !== undefined && storageFile !== undefined
  );

  let {children} = paragraph.data;

  children = children?.map((content: string) => {
    validImages.forEach(({src, storageFile}: SyncStorage) => {
      content = updateImgSrcAlt({data: content, storageFile, imgSrc: src});
    });

    return content;
  });

  return {
    id: paragraph.id,
    data: {
      ...paragraph.data,
      updated_at: new Date(),
      children
    }
  };
};

export const updateSlideChart = ({slide, chart}: {slide: Slide; chart: SyncStorage | undefined}): Slide => {
  if (!chart) {
    return {...slide};
  }

  const {src, storageFile} = chart;

  if (!src || !storageFile) {
    return {...slide};
  }

  const {attributes} = slide.data;

  if (!attributes) {
    return {...slide};
  }

  const {downloadUrl} = storageFile;

  return {
    id: slide.id,
    data: {
      ...slide.data,
      updated_at: new Date(),
      attributes: {
        ...attributes,
        src: downloadUrl
      }
    }
  };
};

const updateImgSrcAlt = ({data, storageFile, imgSrc}: {data: string; storageFile: StorageFile; imgSrc: string}): string => {
  const {downloadUrl, name} = storageFile;

  let updateData: string = data.replaceAll(`img-src="${imgSrc}"`, `img-src="${downloadUrl}"`);
  updateData = updateData.replaceAll(`img-alt="${imgSrc}"`, `img-alt="${name}"`);

  return updateData;
};
