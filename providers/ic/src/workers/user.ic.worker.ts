import {Identity} from '@dfinity/agent';

import {v4 as uuid} from 'uuid';

import {User, UserData} from '@deckdeckgo/editor';

import {_SERVICE as DataBucketActor} from '../canisters/data/data.did';

import {initIdentity} from '../utils/identity.utils';
import {BucketActor, getDataBucket} from '../utils/manager.utils';
import {getData, setData} from '../utils/data.utils';

import {InternetIdentityAuth} from '../types/identity';

export const initUserWorker = async (
  {
    internetIdentity: {delegationChain, identityKey},
    host
  }: {
    internetIdentity: InternetIdentityAuth;
    host: string;
  },
  onInitUserSuccess: (user: User) => Promise<void>
): Promise<User> => {
  if (!delegationChain || !identityKey) {
    return;
  }

  const identity: Identity = initIdentity({identityKey, delegationChain});

  const {actor}: BucketActor<DataBucketActor> = await getDataBucket({identity, host});

  if (!actor) {
    setTimeout(async () => await initUserWorker({internetIdentity: {delegationChain, identityKey}, host}, onInitUserSuccess), 2000);
    return;
  }

  const user: User = await initUser({actor});
  await onInitUserSuccess(user);
};

const initUser = async ({actor}: {actor: DataBucketActor}): Promise<User> => {
  console.log('User IC about to GET');
  const t0 = performance.now();

  const user: User | undefined = await getData<User, UserData>({key: `/user`, actor});

  const t1 = performance.now();
  console.log('User IC GET done', t1 - t0, user);

  if (!user) {
    const newUser: User = await createUser({actor});
    return newUser;
  }

  return user;
};

const createUser = async ({actor}: {actor: DataBucketActor}): Promise<User> => {
  const now: Date = new Date();

  const id: string = uuid();

  const data: UserData = {
    created_at: now,
    updated_at: now
  };

  console.log('User IC about to SET');
  const t0 = performance.now();

  const user: User = await setData<User, UserData>({key: `/user`, id, data, actor});

  const t1 = performance.now();
  console.log('User IC SET done', t1 - t0);

  return user;
};
