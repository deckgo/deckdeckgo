import {DeckPublishData} from '@deckdeckgo/editor';

import {_SERVICE as StorageBucketActor} from '../canisters/storage/storage.did';

export interface StorageUpload {
  actor: StorageBucketActor;
  html: string;
  filename: string;
  pathname: string;
  deckUrl: string;
  bucketUrl: string;
}

export const updateTemplate = ({template, data}: {template: string; data: Partial<DeckPublishData>}): string =>
  Object.entries(data).reduce(
    (acc: string, [key, value]: [string, string]) =>
      acc
        .replaceAll(`{{DECKDECKGO_${key.toUpperCase()}}}`, value || '')
        .replaceAll(`<!-- DECKDECKGO_${key.toUpperCase()} -->`, value || ''),
    template
  );
