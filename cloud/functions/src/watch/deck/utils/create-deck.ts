import * as admin from 'firebase-admin';

import {Deck} from '../../../model/data/deck';
import {SlideData} from '../../../model/data/slide';

import {createDeck as createDeckUtils} from '../../../utils/data/deck-utils';
import {createSlide as createSlideUtils} from '../../../utils/data/slide-utils';

interface MetadataSlide {
  background: string;
  text?: string;
}

const readFile = (filePath: string): Promise<string> => {
  const bucket = admin.storage().bucket();
  const stream = bucket.file(filePath).createReadStream();

  return new Promise<string>((resolve, reject) => {
    let data = '';

    stream.on('data', (chunk: string) => (data += chunk));
    stream.on('end', () => resolve(data));
    stream.on('error', (error: Error) => reject(error));
  });
};

export async function createDeck(objName: string) {
  // 7ZkPkEPNQoVq0Ysylgf2CRFusf32/assets/decks/315731ce-0cf7-479e-8496-3d3015118e47/meta.json
  const split: string[] = objName.split('/');

  const userId: string | undefined = split?.[0];
  const tmpId: string | undefined = split?.[3];

  if (!userId || !tmpId) {
    return;
  }

  const metaContent: string = await readFile(objName);

  if (!metaContent) {
    return;
  }

  const metaList: MetadataSlide[] = JSON.parse(metaContent);

  if (!metaList || metaList.length <= 0) {
    return;
  }

  const deck: Deck = await createDeckUtils(userId);

  const promises: Promise<void>[] = metaList.map((meta: MetadataSlide) => createSlide(deck, meta, userId, tmpId));
  await Promise.all(promises);

  // TODO: This cannot happens here, text by text should be deleted
  await cleanStorage(userId, tmpId);
}

const createSlide = async ({id}: Deck, meta: MetadataSlide, userId: string, tmpId: string) => {
  const bucket = admin.storage().bucket();

  // TODO: on unzip background should be added to the images folder
  const backgroundPath: string = `${userId}/assets/decks/${tmpId}/${meta.background}`;

  const file = bucket.file(backgroundPath);
  await file.move(backgroundPath);

  const downloadUrl: string | undefined = (await file.getMetadata())?.[0].mediaLink;

  const content: string | undefined = meta.text ? await readFile(`${userId}/assets/decks/${id}/${meta.text}`) : undefined;

  const slideData: SlideData = {
    template: 'svg',
    ...(content && {content}),
    ...(downloadUrl && {
      attributes: {
        src: downloadUrl,
      },
    }),
  };

  await createSlideUtils(id, slideData);
};

const cleanStorage = async (userId: string, tmpId: string) => {
  const tmpPath: string = `${userId}/assets/decks/${tmpId}/`;

  const bucket = admin.storage().bucket();
  const folder = bucket.file(tmpPath);

  await folder.delete();
};
