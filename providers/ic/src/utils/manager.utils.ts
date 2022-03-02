import {Identity} from '@dfinity/agent';
import {IDL} from '@dfinity/candid';
import {Principal} from '@dfinity/principal';
import {_SERVICE as DataBucketActor} from '../canisters/data/data.did';
import {idlFactory as DataIdlFactory} from '../canisters/data/data.utils.did';
import {Bucket, _SERVICE as ManagerActor} from '../canisters/manager/manager.did';
import {idlFactory as ManagerFactory} from '../canisters/manager/manager.utils.did';
import {_SERVICE as StorageBucketActor} from '../canisters/storage/storage.did';
import {idlFactory as StorageIdlFactory} from '../canisters/storage/storage.utils.did';
import {createActor} from './actor.utils';
import {fromNullable} from './did.utils';

export interface BucketActor<T> {
  bucketId: Principal | undefined;
  actor: T | undefined;
}

export const createManagerActor = ({identity}: {identity: Identity}): Promise<ManagerActor> => {
  return createActor<ManagerActor>({canisterId: process.env.MANAGER_CANISTER_ID, idlFactory: ManagerFactory, identity});
};

export const getDataBucket = async ({identity}: {identity: Identity | undefined}): Promise<BucketActor<DataBucketActor>> => {
  return getBucket<DataBucketActor | undefined>({identity, idlFactory: DataIdlFactory, initBucket: initDataBucket});
};

export const getStorageBucket = async ({identity}: {identity: Identity | undefined}): Promise<BucketActor<StorageBucketActor>> => {
  return getBucket<StorageBucketActor>({identity, idlFactory: StorageIdlFactory, initBucket: initStorageBucket});
};

const getBucket = async <T>({
  identity,
  idlFactory,
  initBucket
}: {
  identity: Identity | undefined;
  idlFactory: IDL.InterfaceFactory;
  initBucket: ({managerActor}: {managerActor: ManagerActor}) => Promise<Bucket>;
}): Promise<BucketActor<T>> => {
  if (!identity) {
    throw new Error('Invalid identity.');
  }

  const managerActor: ManagerActor = await createManagerActor({identity});

  const bucket: Bucket = await initBucket({managerActor});

  const bucketId: Principal | undefined = fromNullable<Principal>(bucket.bucketId);

  const actor: T | undefined = bucketId ? await createBucketActor<T>({identity, bucketId, idlFactory}) : undefined;

  return {
    bucketId,
    actor
  };
};

const createBucketActor = <T>({
  identity,
  bucketId,
  idlFactory
}: {
  identity: Identity;
  bucketId: Principal;
  idlFactory: IDL.InterfaceFactory;
}): Promise<T> => {
  return createActor<T>({canisterId: bucketId, idlFactory, identity});
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
