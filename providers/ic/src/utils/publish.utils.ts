import {Principal} from '@dfinity/principal';
import {Identity} from '@dfinity/agent';

import {DeckPublishData} from '@deckdeckgo/editor';

import {_SERVICE as StorageBucketActor} from '../canisters/storage/storage.did';

import {getIdentity} from '../providers/auth/auth.ic';

import {getStorageBucket} from './manager.utils';

export interface StorageUpload {
  actor: StorageBucketActor;
  html: string;
  filename: string;
  pathname: string;
  deckUrl: string;
  bucketUrl: string;
}

export const getPublishBucket = (): Promise<{bucket: Principal; actor: StorageBucketActor}> => {
  const identity: Identity | undefined = getIdentity();
  return getStorageBucket({identity});
};

export const updateTemplate = ({template, deckPublishData}: {template: string; deckPublishData: DeckPublishData}): string =>
  Object.entries(deckPublishData).reduce(
    (acc: string, [key, value]: [string, string]) =>
      acc
        .replaceAll(`{{DECKDECKGO_${key.toUpperCase()}}}`, value || '')
        .replaceAll(`<!-- DECKDECKGO_${key.toUpperCase()} -->`, value || ''),
    template
  );
