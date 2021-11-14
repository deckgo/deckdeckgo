import {Identity} from '@dfinity/agent';
import {Principal} from '@dfinity/principal';

import {v4 as uuid} from 'uuid';

import {User, UserData} from '@deckdeckgo/editor';

import {_SERVICE as DataBucketActor} from '../canisters/data/data.did';

import {toTimestamp} from '../utils/did.utils';
import {initIdentity} from '../utils/identity.utils';
import {getDataBucket} from '../utils/manager.utils';
import {getData, setData} from '../utils/data.utils';

import {InternetIdentityAuth} from '../types/identity';

export const initUserWorker = async ({
  internetIdentity: {delegationChain, identityKey},
  host
}: {
  internetIdentity: InternetIdentityAuth;
  host: string;
}): Promise<User> => {
  if (!delegationChain || !identityKey) {
    return;
  }

  const identity: Identity = initIdentity({identityKey, delegationChain});

  const {actor}: {bucket: Principal; actor: DataBucketActor} = await getDataBucket({identity, host});

  console.log('User IC about to GET');
  const t0 = performance.now();

  const user: User | undefined = await getData<User, UserData>({key: `/user`});

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
    anonymous: false,
    created_at: toTimestamp(now),
    updated_at: toTimestamp(now)
  };

  console.log('User IC about to SET');
  const t0 = performance.now();

  const user: User = await setData<User, UserData>({key: `/user`, id, data, actor});

  const t1 = performance.now();
  console.log('User IC SET done', t1 - t0);

  return user;
};
