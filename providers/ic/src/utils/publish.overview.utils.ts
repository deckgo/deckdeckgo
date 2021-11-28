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

  const {photo_url, ...data} = deckPublishData;

  let html: string = updateTemplate({template, data});
  html = updatePhotoUrl({html, photo_url});
  html = updateDeckList({deckId, template: html, deckPublishData, storageUpload});

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

const updatePhotoUrl = ({photo_url, html}: {photo_url: string | undefined; html: string}): string => {
  if (!photo_url) {
    return html;
  }

  const photoUrlRegExp: RegExp = new RegExp('<!-- DECKDECKGO_PHOTO_URL -->.*?\\/>');

  const alreadyContainsPhotoUrl: boolean = photoUrlRegExp.test(html);

  const img: string = `<img loading="lazy" src="${photo_url}" />`;

  if (alreadyContainsPhotoUrl) {
    return html.replace(photoUrlRegExp, img);
  }

  return html.replace('<!-- DECKDECKGO_PHOTO_URL -->', img);
};

const updateDeckList = ({
  deckId,
  template,
  storageUpload,
  deckPublishData
}: {
  deckId: string;
  template: string;
  storageUpload: StorageUpload;
  deckPublishData: DeckPublishData;
}): string => {
  const deckRegExp: RegExp = new RegExp(`<li.*?deck-id="${deckId}".*?li>`);

  const alreadyPublished: boolean = deckRegExp.test(template);

  const {title} = deckPublishData;
  const {deckUrl} = storageUpload;

  const li: string = `<li deck-id="${deckId}"><a href="${deckUrl}">${title}</a></li>`;

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
