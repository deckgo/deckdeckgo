import {Identity} from '@dfinity/agent';
import {Principal} from '@dfinity/principal';

import {_SERVICE as DataBucketActor, Data} from '../canisters/data/data.did';

import {getIdentity} from '../providers/auth/auth.ic';

import {fromArray, fromTimestamp, toNullable} from './did.utils';
import {getDataBucket} from './manager.utils';

export const entries = async <T, D>({filter}: {filter?: string}): Promise<T[]> => {
  const identity: Identity | undefined = getIdentity();

  if (!identity) {
    return [];
  }

  console.log('Data IC about to request entries');
  const t0 = performance.now();

  const {actor}: {bucket: Principal; actor: DataBucketActor} = await getDataBucket({identity});

  const data: Data[] = await actor.list(toNullable<string>(filter));

  const promises: Promise<T>[] = data.map((data: Data) => fromData<T, D>({data, identity}));
  const datas: T[] = await Promise.all(promises);

  const t1 = performance.now();
  console.log(`Data IC datas done. ${t1 - t0}`, datas);

  return datas;
};

const fromData = async <T, D>({data, identity}: {data: Data; identity: Identity}): Promise<T> => {
  const dataData: D = await fromArray<D>(data.data);

  return {
    id: data.id,
    data: {
      ...dataData,
      owner_id: identity.getPrincipal().toText(),
      created_at: fromTimestamp(data.created_at),
      updated_at: fromTimestamp(data.updated_at)
    }
  } as unknown as T;
};

export const deleteEntry = async ({key}: {key: string}): Promise<void> => {
  if (!key) {
    return;
  }

  const identity: Identity | undefined = getIdentity();

  if (!identity) {
    return;
  }

  console.log('Data IC about to delete data and its slides');

  const {actor}: {bucket: Principal; actor: DataBucketActor} = await getDataBucket({identity});

  await actor.del(key);

  console.log('Data IC delete');
};
