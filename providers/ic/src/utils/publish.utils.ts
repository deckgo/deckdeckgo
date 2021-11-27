import {Principal} from '@dfinity/principal';
import {Identity} from '@dfinity/agent';

import {_SERVICE as StorageBucketActor} from '../canisters/storage/storage.did';

import {getIdentity} from '../providers/auth/auth.ic';

import {getStorageBucket} from './manager.utils';

export const getPublishBucket = (): Promise<{bucket: Principal; actor: StorageBucketActor}> => {
  const identity: Identity | undefined = getIdentity();
  return getStorageBucket({identity});
};
