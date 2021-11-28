import {DeckPublishData} from '@deckdeckgo/editor';

import {_SERVICE as StorageBucketActor} from '../canisters/storage/storage.did';

import {StorageUpload, updateTemplate} from './publish.utils';
import {upload} from './storage.utils';

export const publishOverview = async ({
  deckId,
  storageUpload,
  deckPublishData
}: {
  deckId: string;
  storageUpload: StorageUpload;
  deckPublishData: DeckPublishData;
}): Promise<void> => {
  const template: string = await htmlSource(storageUpload);

  let html: string = updateTemplate({template, deckPublishData});

  html = updateDeckList({deckId, template: html, deckPublishData});

  const {actor} = storageUpload;

  await uploadOverviewIC({html, actor});
};

const htmlSource = async ({bucketUrl}: StorageUpload): Promise<string> => {
  const response: Response = await fetch(bucketUrl);

  if (response.ok) {
    return response.text();
  }

  return await htmlTemplate();
};

const htmlTemplate = async (): Promise<string> => {
  const htmlTemplate: Response = await fetch('https://raw.githubusercontent.com/deckgo/ic-kit/main/dist/index.html');
  return htmlTemplate.text();
};

const updateDeckList = ({
  deckId,
  template,
  deckPublishData
}: {
  deckId: string;
  template: string;
  deckPublishData: DeckPublishData;
}): string => {
  const deckRegExp: RegExp = new RegExp(`<li.*?deck-id="${deckId}".*?li>`);

  const alreadyPublished: boolean = deckRegExp.test(template);

  const {title} = deckPublishData;

  const li: string = `<li deck-id="${deckId}">${title}</li>`;

  if (alreadyPublished) {
    return template.replace(deckRegExp, li);
  }

  return template.replace(/<!-- DECKDECKGO_DATA -->/, `${li}<!-- DECKDECKGO_DATA -->`);
};

const uploadOverviewIC = async ({html, actor}: {html: string; actor: StorageBucketActor}): Promise<void> => {
  await upload({
    data: new Blob([html], {type: 'text/html'}),
    filename: 'index.html',
    folder: 'resources',
    storageBucket: actor,
    headers: [['Cache-Control', 'max-age=3600']],
    fullPath: '/'
  });
};
