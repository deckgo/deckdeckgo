import {StorageUpload, updateTemplate} from './publish.utils';
import {DeckPublishData} from '@deckdeckgo/editor';

export const publishOverview = async ({
  deckId,
  storageUpload,
  deckPublishData
}: {
  deckId: string;
  storageUpload: StorageUpload;
  deckPublishData: DeckPublishData;
}): Promise<void> => {
  const template: string = await html(storageUpload);

  let updatedTemplate: string = updateTemplate({template, deckPublishData});

  // TODO
  console.log(updatedTemplate, deckId);
};

const html = async ({bucketUrl}: StorageUpload): Promise<string> => {
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
