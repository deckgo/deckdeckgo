import {Identity} from '@dfinity/agent';
import {Principal} from '@dfinity/principal';
import {IDL} from '@dfinity/candid';

import {idlFactory as ManagerFactory} from '../canisters/manager/manager.utils.did';
import {_SERVICE as ManagerActor, Bucket} from '../canisters/manager/manager.did';

import {_SERVICE as DataBucketActor} from '../canisters/data/data.did';
import {idlFactory as DataIdlFactory} from '../canisters/data/data.utils.did';

import {_SERVICE as StorageBucketActor} from '../canisters/storage/storage.did';
import {idlFactory as StorageIdlFactory} from '../canisters/storage/storage.utils.did';

import {createActor} from './actor.utils';
import {fromNullable} from './did.utils';

export interface BucketActor<T> {
  bucketId: Principal | undefined;
  actor: T | undefined;
}

export const createManagerActor = ({identity, host}: {identity: Identity; host?: string}): Promise<ManagerActor> => {
  return createActor<ManagerActor>({canisterId: process.env.MANAGER_CANISTER_ID, idlFactory: ManagerFactory, identity, host});
};

export const getDataBucket = async ({
  host,
  identity
}: {
  host?: string;
  identity: Identity | undefined;
}): Promise<BucketActor<DataBucketActor>> => {
  return getBucket<DataBucketActor | undefined>({host, identity, idlFactory: DataIdlFactory, initBucket: initDataBucket});
};

export const getStorageBucket = async ({
  host,
  identity
}: {
  host?: string;
  identity: Identity | undefined;
}): Promise<BucketActor<StorageBucketActor>> => {
  return getBucket<StorageBucketActor>({host, identity, idlFactory: StorageIdlFactory, initBucket: initStorageBucket});
};

const getBucket = async <T>({
  host,
  identity,
  idlFactory,
  initBucket
}: {
  host?: string;
  identity: Identity | undefined;
  idlFactory: IDL.InterfaceFactory;
  initBucket: ({managerActor}: {managerActor: ManagerActor}) => Promise<Bucket>;
}): Promise<BucketActor<T>> => {
  if (!identity) {
    throw new Error('Invalid identity.');
  }

  const managerActor: ManagerActor = await createManagerActor({identity, host});

  const bucket: Bucket = await initBucket({managerActor});

  const bucketId: Principal | undefined = fromNullable<Principal>(bucket.bucketId);

  const actor: T | undefined = bucketId ? await createBucketActor<T>({identity, bucketId, idlFactory, host}) : undefined;

  return {
    bucketId,
    actor
  };
};

const createBucketActor = <T>({
  identity,
  bucketId,
  idlFactory,
  host
}: {
  identity: Identity;
  bucketId: Principal;
  idlFactory: IDL.InterfaceFactory;
  host?: string;
}): Promise<T> => {
  return createActor<T>({canisterId: bucketId, idlFactory, identity, host});
};

const initDataBucket = async ({managerActor}: {managerActor: ManagerActor}): Promise<Bucket> => {
  const existingBucket: Bucket | undefined = fromNullable<Bucket>(await managerActor.getData());

  if (!existingBucket) {
    return await managerActor.initData();
  }

  return existingBucket;
};

const initStorageBucket = async ({managerActor}: {managerActor: ManagerActor}): Promise<Bucket> => {
  const existingBucket: Bucket | undefined = fromNullable<Bucket>(await managerActor.getStorage());

  if (!existingBucket) {
    return await managerActor.initStorage();
  }

  return existingBucket;
};
