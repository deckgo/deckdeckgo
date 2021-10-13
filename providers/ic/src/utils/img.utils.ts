import {Deck, StorageFile} from '@deckdeckgo/editor';

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

const updateImgSrcAlt = ({data, storageFile, imgSrc}: {data: string; storageFile: StorageFile; imgSrc: string}): string => {
  const {downloadUrl, name} = storageFile;

  let updateData: string = data.replace(`img-src="${imgSrc}"`, `img-src="${downloadUrl}"`);
  updateData = updateData.replace(`img-alt="${imgSrc}"`, `img-alt="${name}"`);

  return updateData;
};
