import {User, UserData} from '@deckdeckgo/editor';
import {Identity} from '@dfinity/agent';
import {nanoid} from 'nanoid';
import {_SERVICE as DataBucketActor} from '../canisters/data/data.did';
import {InternetIdentityAuth} from '../types/identity';
import {LogWindow} from '../types/sync.window';
import {getData, setData} from '../utils/data.utils';
import {initIdentity} from '../utils/identity.utils';
import {BucketActor, getDataBucket} from '../utils/manager.utils';

export const initUserWorker = (
  {
    internetIdentity
  }: {
    internetIdentity: InternetIdentityAuth;
  },
  onInitUserSuccess: (user: User) => Promise<void>,
  log: LogWindow
): Promise<void> => initUser({internetIdentity}, onInitUserSuccess, log);

const initUser = async (
  {
    internetIdentity: {delegationChain, identityKey}
  }: {
    internetIdentity: InternetIdentityAuth;
  },
  onInitUserSuccess: (user: User) => Promise<void>,
  log: LogWindow
) =>
  new Promise<void>(async (resolve) => {
    if (!delegationChain || !identityKey) {
      resolve();
      return;
    }

    const identity: Identity = initIdentity({identityKey, delegationChain});

    const {actor}: BucketActor<DataBucketActor> = await getDataBucket({identity});

    if (!actor) {
      setTimeout(async () => {
        await initUser({internetIdentity: {delegationChain, identityKey}}, onInitUserSuccess, log);
        resolve();
      }, 2000);
      return;
    }

    const user: User = await initUserData({actor, log});
    await onInitUserSuccess(user);

    resolve();
  });

const initUserData = async ({actor, log}: {actor: DataBucketActor; log: LogWindow}): Promise<User> => {
  log({msg: `[get][start] user`});
  const t0 = performance.now();

  const user: User | undefined = await getData<User, UserData>({key: `/user`, actor});

  const t1 = performance.now();
  log({msg: `[get][done] user`, duration: t1 - t0});

  if (!user) {
    const newUser: User = await createUser({actor, log});
    return newUser;
  }

  return user;
};

const createUser = async ({actor, log}: {actor: DataBucketActor; log: LogWindow}): Promise<User> => {
  const now: Date = new Date();

  const id: string = nanoid();

  const data: UserData = {
    created_at: now,
    updated_at: now
  };

  const user: User = await setData<User, UserData>({key: `/user`, id, data, actor, log});
  return user;
};
