import {Identity} from '@dfinity/agent';
import {Principal} from '@dfinity/principal';

import {Deck, DeckData, Publish, DeckPublishData, publishData} from '@deckdeckgo/editor';

import {_SERVICE as StorageBucketActor} from '../../canisters/storage/storage.did';

import {getIdentity} from '../auth/auth.ic';

import {setData} from '../../utils/data.utils';
import {getStorageBucket} from '../../utils/manager.utils';
import {upload} from '../../utils/storage.utils';

interface StorageUpload {
  actor: StorageBucketActor;
  html: string;
  filename: string;
  pathname: string;
}

export const publish: Publish = async ({deck: deckSource}: {deck: Deck; config: Record<string, string>}): Promise<Deck> => {
  const {id, data} = deckSource;

  // 1. Init and fill HTML
  const uploadData: StorageUpload = await initUpload({deck: deckSource});

  // 2. Update deck published meta
  const publishData: DeckData = updateDeckMetaData({data, uploadData});

  // 3. Update deck meta information
  const deck: Deck = await setData<Deck, DeckData>({key: `/decks/${id}`, id, data: publishData});

  // 4. Upload
  await uploadFileIC(uploadData);

  // 5. Tells the snapshot the process is over
  emitDeckPublished(deck);

  return deck;
};

const initUpload = async ({deck}: {deck: Deck}): Promise<StorageUpload> => {
  const {html, deckPublishData}: {html: string; deckPublishData: DeckPublishData} = await initIndexHTML({deck});

  // 1. Get actor
  const identity: Identity | undefined = getIdentity();
  const {bucket, actor}: {bucket: Principal; actor: StorageBucketActor} = await getStorageBucket({identity});

  // 2. Folder and filename
  const folder: string = 'static';
  const filename: string = encodeURI(deckPublishData.title);
  const url: string = `https://${bucket.toText()}.raw.ic0.app/${folder}/${filename}`;

  // 3. Update URL
  const indexHTML = html.replace('{{DECKDECKGO_URL}}', url);

  return {
    html: indexHTML,
    actor,
    filename,
    pathname: new URL(url).pathname
  };
};

const initIndexHTML = async ({deck}: {deck: Deck}): Promise<{html: string; deckPublishData: DeckPublishData}> => {
  const deckPublishData: DeckPublishData = publishData({deck});

  const template: string = await htmlTemplate();

  let updatedTemplate: string = Object.entries(deckPublishData).reduce((acc: string, [key, value]: [string, string]) => {
    acc.replaceAll(`{{DECKDECKGO_${key.toUpperCase()}}}`, value || '');
    acc.replaceAll(`<!-- DECKDECKGO_${key.toUpperCase()} -->`, value || '');
    return acc;
  }, template);

  const {attributes, slides} = deckPublishData;

  const attr: string = attributes
    ? Object.entries(deckPublishData).reduce((acc: string, [key, value]: [string, string]) => `${acc}; ${key}: ${value}`, '')
    : undefined;

  updatedTemplate = updatedTemplate.replace(
    '<!-- DECKDECKGO_DECK -->',
    `<deckgo-deck id="slider" embedded="true" ${attr || ''}>${slides.map((slide: string) => slide)}</deckgo-deck>`
  );

  return {
    html: updatedTemplate,
    deckPublishData
  };
};

const htmlTemplate = async (): Promise<string> => {
  const htmlTemplate: Response = await fetch('https://raw.githubusercontent.com/deckgo/ic-kit/main/dist/deck.html');
  return htmlTemplate.text();
};

const uploadFileIC = async ({filename, html, actor}: {filename: string; html: string; actor: StorageBucketActor}): Promise<void> => {
  await upload({
    data: new Blob([JSON.stringify(html)], {type: 'application/json; charset=utf-8'}),
    filename,
    folder: 'static',
    storageBucket: actor
  });
};

const emitDeckPublished = (deck: Deck) => {
  const {id, data} = deck;

  const deployedDeck: Deck = {
    id,
    data: {
      ...data,
      deploy: {
        api: {
          status: 'successful',
          updated_at: new Date()
        }
      }
    }
  };

  const $event: CustomEvent<Deck> = new CustomEvent('deckPublished', {detail: deployedDeck});
  document.dispatchEvent($event);
};

const updateDeckMetaData = ({data, uploadData}: {data: DeckData; uploadData: StorageUpload}): DeckData => {
  const {pathname} = uploadData;

  const now: Date = new Date();

  return {
    ...data,
    meta: {
      ...(data.meta || {title: data.name}),
      pathname,
      published: true,
      published_at: now,
      updated_at: now
    }
  };
};
