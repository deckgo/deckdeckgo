import {Identity} from '@dfinity/agent';
import {Data, DataFilter, _SERVICE as DataBucketActor} from '../canisters/data/data.did';
import {getIdentity} from '../providers/auth/auth.ic';
import {LogWindow} from '../types/sync.window';
import {fromArray, fromNullable, fromTimestamp, toArray, toNullable, toTimestamp} from './did.utils';
import {BucketActor, getDataBucket} from './manager.utils';

export const entries = async <T, D>({startsWith, notContains}: {startsWith?: string; notContains?: string}): Promise<T[]> => {
  const identity: Identity | undefined = getIdentity();

  if (!identity) {
    return [];
  }

  const {actor}: BucketActor<DataBucketActor> = await getDataBucket({identity});

  if (!actor) {
    return [];
  }

  const data: [string, Data][] = await actor.list(
    toNullable<DataFilter>({
      startsWith: toNullable<string>(startsWith),
      notContains: toNullable<string>(notContains)
    })
  );

  const promises: Promise<T>[] = data.map(([, data]: [string, Data]) => fromData<T, D>({data, identity}));
  const results: T[] = await Promise.all(promises);

  return results;
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

export const deleteData = async ({key, actor, log}: {key: string; actor?: DataBucketActor; log?: LogWindow}): Promise<void> => {
  if (!key) {
    return;
  }

  log?.({msg: `[delete][start] ${key}`});
  const t0 = performance.now();

  const dataActor: DataBucketActor = actor || (await getDataActor());

  await dataActor.del(key);

  const t1 = performance.now();
  log?.({msg: `[delete][done] ${key}`, duration: t1 - t0});
};

export const getData = <T, D>({key, actor}: {key: string; actor?: DataBucketActor}): Promise<T | undefined> => {
  return new Promise<T | undefined>(async (resolve, reject) => {
    try {
      const dataActor: DataBucketActor = actor || (await getDataActor());

      const entry: Data | undefined = fromNullable<Data>(await dataActor.get(key));

      if (!entry) {
        resolve(undefined);
        return;
      }

      const data: D = await fromArray<D>(entry.data);

      resolve({
        id: entry.id,
        data: {
          ...data,
          created_at: fromTimestamp(entry.created_at),
          updated_at: fromTimestamp(entry.updated_at)
        }
      } as unknown as T);
    } catch (err) {
      reject(err);
    }
  });
};

export const setData = <T, D>({
  key,
  data,
  id,
  actor = undefined,
  log
}: {
  key: string;
  data: D;
  id: string;
  actor?: DataBucketActor;
  log?: LogWindow;
}): Promise<T> => {
  return new Promise<T>(async (resolve, reject) => {
    try {
      log?.({msg: `[set][start] ${key}`});
      const t0 = performance.now();

      const dataActor: DataBucketActor = actor || (await getDataActor());

      const now: Date = new Date();

      await dataActor.set(key, {
        id,
        data: await toArray<D>(data),
        created_at: toTimestamp((data as unknown as {created_at: Date}).created_at || new Date()),
        updated_at: toTimestamp(now)
      });

      const t1 = performance.now();
      log?.({msg: `[set][done] ${key}`, duration: t1 - t0});

      const result: T = {
        id,
        data: {
          ...data,
          updated_at: now
        }
      } as unknown as T;

      resolve(result);
    } catch (err) {
      reject(err);
    }
  });
};

const getDataActor = async (): Promise<DataBucketActor> => {
  const identity: Identity | undefined = getIdentity();

  if (!identity) {
    throw new Error('No internet identity.');
  }

  const {actor}: BucketActor<DataBucketActor> = await getDataBucket({identity});

  if (!actor) {
    throw new Error('No actor initialized.');
  }

  return actor;
};
