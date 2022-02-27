import {log, PublishData} from '@deckdeckgo/editor';
import {_SERVICE as StorageBucketActor} from '../canisters/storage/storage.did';
import {StorageUpload, updateTemplate} from './publish.utils';
import {upload} from './storage.utils';

export const publishOverview = async ({
  dataId,
  storageUpload,
  publishData
}: {
  dataId: string;
  storageUpload: StorageUpload;
  publishData: PublishData;
}): Promise<void> => {
  const template: string = await htmlTemplate();

  const {photo_url, ...data} = publishData;

  let html: string = updateTemplate({template, data});
  html = updatePhotoUrl({html, photo_url});

  html = await updateList({dataId, template: html, publishData, storageUpload});

  const {actor} = storageUpload;

  await uploadOverviewIC({html, actor});
};

const htmlTemplate = async (): Promise<string> => {
  const htmlTemplate: Response = await fetch('https://raw.githubusercontent.com/deckgo/ic-kit/main/dist/index.html');
  return htmlTemplate.text();
};

const updatePhotoUrl = ({photo_url, html}: {photo_url: string | undefined; html: string}): string => {
  if (!photo_url) {
    return html;
  }

  return html.replace('<!-- DECKDECKGO_PHOTO_URL -->', `<img role="presentation" alt="" loading="lazy" src="${photo_url}" />`);
};

const updateList = async ({
  dataId,
  template,
  storageUpload,
  publishData
}: {
  dataId: string;
  template: string;
  storageUpload: StorageUpload;
  publishData: PublishData;
}): Promise<string> => {
  const {title} = publishData;
  const {fullUrl} = storageUpload;

  const li: string = `<li data-id="${dataId}"><a href="${fullUrl}">${title}</a></li>`;

  const source: string | undefined = await htmlSource(storageUpload);

  if (!source) {
    return template.replace(/<!-- DECKDECKGO_DATA -->/, `${li}`);
  }

  const matches: RegExpMatchArray[] = [...source.matchAll(/<li\x20.*?data-id=".*?".*?li>/gm)];

  if (!matches || matches.length <= 0) {
    return template.replace(/<!-- DECKDECKGO_DATA -->/, `${li}`);
  }

  const list: string[] = matches.map((match: RegExpMatchArray) => match[0]);

  const index: number = list.indexOf(li);

  if (index > -1) {
    return template.replace(/<!-- DECKDECKGO_DATA -->/, list.map((entry: string, i: number) => (i === index ? li : entry)).join(''));
  }

  return template.replace(/<!-- DECKDECKGO_DATA -->/, [li, ...list].join(''));
};

const htmlSource = async ({bucketUrl}: StorageUpload): Promise<string | undefined> => {
  const response: Response = await fetch(bucketUrl);

  if (response.ok) {
    return response.text();
  }

  return undefined;
};

const uploadOverviewIC = async ({html, actor}: {html: string; actor: StorageBucketActor}): Promise<void> => {
  await upload({
    data: new Blob([html], {type: 'text/html'}),
    filename: 'index.html',
    folder: 'resources',
    storageActor: actor,
    headers: [['Cache-Control', 'max-age=0']],
    fullPath: '/',
    log
  });
};
