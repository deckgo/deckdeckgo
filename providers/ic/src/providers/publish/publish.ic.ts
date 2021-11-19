import {Deck, DeckData, Publish, DeckPublishData, publishData} from '@deckdeckgo/editor';

import {setData} from '../../utils/data.utils';
import {_SERVICE as StorageBucketActor} from '../../canisters/storage/storage.did';

export const publish: Publish = async ({deck: deckSource}: {deck: Deck; config: Record<string, string>}): Promise<Deck> => {
  const {id, data} = deckSource;

  // 1. Update deck meta information
  const deck: Deck = await setData<Deck, DeckData>({key: `/decks/${id}`, id, data});

  // 2. Trigger the function that effectively publish
  await publishDeck({deck});

  return deck;
};

const publishDeck = async ({deck}: {deck: Deck}): Promise<void> => {
  const deckPublishData: DeckPublishData = publishData({deck});

  const template: string = await htmlTemplate();

  let updatedTemplate: string = Object.entries<string>(deckPublishData).reduce((acc: string, [key, value]: [string, string]) => {
    acc.replaceAll(`{{DECKDECKGO_${key.toUpperCase()}}}`, value || '');
    acc.replaceAll(`<!-- DECKDECKGO_${key.toUpperCase()} -->`, value || '');
    return acc;
  }, template);

  // <deckgo-deck id="slider" embedded="true"> </deckgo-deck>
  const {attributes, slides} = deckPublishData;

  const attr: string = attributes
    ? Object.entries<string>(deckPublishData).reduce((acc: string, [key, value]: [string, string]) => `${acc}; ${key}: ${value}`, '')
    : undefined;

  updatedTemplate = updatedTemplate.replace(
    '<!-- DECKDECKGO_DECK -->',
    `<deckgo-deck id="slider" embedded="true" ${attr || ''}>${slides.map((slide: string) => slide)}</deckgo-deck>`
  );

  // TODO DECKDECKGO_BASE_HREF
};

const upload = async ({
  data,
  folder,
  storageBucket
}: {
  data: File;
  folder: string;
  storageBucket: StorageBucketActor;
}): Promise<{fullPath: string; filename: string; token: string}> => {
  const filename: string = encodeURI(data.name);
  const fullPath: string = `${folder}/${filename}`;
  const token: string = uuid();

  console.log('About to upload to the IC');
  const t0 = performance.now();

  const {batchId} = await storageBucket.create_batch({name: filename, fullPath, token, folder});

  const t1 = performance.now();
  console.log('Upload create_batch', t1 - t0);

  const promises = [];

  const chunkSize = 700000;

  for (let start = 0; start < data.size; start += chunkSize) {
    const chunk: Blob = data.slice(start, start + chunkSize);

    promises.push(
      uploadChunk({
        batchId,
        chunk,
        storageBucket
      })
    );
  }

  const chunkIds: {chunkId: bigint}[] = await Promise.all(promises);

  const t2 = performance.now();
  console.log('Upload upload chunks', t2 - t1);

  await storageBucket.commit_batch({
    batchId,
    chunkIds: chunkIds.map(({chunkId}: {chunkId: bigint}) => chunkId),
    contentType: data.type
  });

  const t3 = performance.now();
  console.log('Upload commit_batch', t3 - t2);
  console.log('Data uploaded', t3 - t0);

  return {
    fullPath,
    filename,
    token
  };
};

const htmlTemplate = async (): Promise<string> => {
  const htmlTemplate: Response = await fetch('https://raw.githubusercontent.com/deckgo/ic-kit/main/dist/deck.html');
  return htmlTemplate.text();
};
