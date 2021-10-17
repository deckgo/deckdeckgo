import {Identity} from '@dfinity/agent';
import {Principal} from '@dfinity/principal';

import {v4 as uuid} from 'uuid';

import {GetFiles, StorageFile, StorageFilesList, UploadFile, DeleteFile} from '@deckdeckgo/editor';

import {getIdentity} from '../auth/auth.ic';

import {_SERVICE as ManagerActor} from '../../canisters/manager/manager.did';
import {_SERVICE as StorageBucketActor, AssetKey} from '../../canisters/storage/storage.did';

import {createManagerActor, createStorageBucketActor, initStorageBucket} from '../../utils/manager.utils';
import {toNullable} from '../../utils/did.utils';

export const uploadFile: UploadFile = async ({
  data,
  folder,
  maxSize
}: {
  data: File;
  folder: string;
  maxSize: number;
  userId: string;
  downloadUrl?: boolean;
}): Promise<StorageFile | undefined> => {
  const identity: Identity | undefined = getIdentity();

  return uploadFileIC({data, folder, maxSize, identity});
};

export const uploadFileIC = async ({
  data,
  maxSize,
  host,
  folder,
  identity
}: {
  data: File;
  folder: string;
  maxSize: number;
  host?: string;
  identity: Identity;
}): Promise<StorageFile | undefined> => {
  if (!data || !data.name) {
    throw new Error('File not valid.');
  }

  if (data.size > maxSize) {
    throw new Error(`File is too big (max. ${maxSize / 1048576} Mb)`);
  }

  const {bucket, actor}: {bucket: Principal; actor: StorageBucketActor} = await getStorageBucket({host, identity});

  const {fullPath, filename, token}: {fullPath: string; filename: string; token: string} = await upload({
    data,
    folder,
    storageBucket: actor
  });

  return {
    downloadUrl: `https://${bucket.toText()}.raw.ic0.app/${fullPath}?token=${token}`,
    fullPath,
    name: filename
  };
};

const getStorageBucket = async ({
  host,
  identity
}: {
  host?: string;
  identity: Identity | undefined;
}): Promise<{bucket: Principal; actor: StorageBucketActor}> => {
  if (!identity) {
    throw new Error('Invalid identity.');
  }

  const managerActor: ManagerActor = await createManagerActor({identity, host});

  const bucket: Principal = await initStorageBucket({managerActor});

  const actor: StorageBucketActor = await createStorageBucketActor({identity, bucket, host});

  return {
    bucket,
    actor
  };
};

const uploadChunk = async ({
  batchId,
  chunk,
  storageBucket
}: {
  batchId: bigint;
  chunk: Blob;
  storageBucket: StorageBucketActor;
}): Promise<{chunkId: bigint}> =>
  storageBucket.create_chunk({
    batchId,
    content: [...new Uint8Array(await chunk.arrayBuffer())]
  });

const upload = async ({
  data,
  folder,
  storageBucket
}: {
  data: File;
  folder: string;
  storageBucket: StorageBucketActor;
}): Promise<{fullPath: string; filename: string; token: string}> => {
  const filename: string = encodeURI(data.name);
  const fullPath: string = `${folder}/${filename}`;
  const token: string = uuid();

  console.log('About to upload to the IC');
  const t0 = performance.now();

  const {batchId} = await storageBucket.create_batch({name: filename, fullPath, token, folder});

  const t1 = performance.now();
  console.log('Upload create_batch', t1 - t0);

  const promises = [];

  const chunkSize = 700000;

  for (let start = 0; start < data.size; start += chunkSize) {
    const chunk: Blob = data.slice(start, start + chunkSize);

    promises.push(
      uploadChunk({
        batchId,
        chunk,
        storageBucket
      })
    );
  }

  const chunkIds: {chunkId: bigint}[] = await Promise.all(promises);

  const t2 = performance.now();
  console.log('Upload upload chunks', t2 - t1);

  await storageBucket.commit_batch({
    batchId,
    chunkIds: chunkIds.map(({chunkId}: {chunkId: bigint}) => chunkId),
    contentType: data.type
  });

  const t3 = performance.now();
  console.log('Upload commit_batch', t3 - t2);
  console.log('Data uploaded', t3 - t0);

  return {
    fullPath,
    filename,
    token
  };
};

export const getFiles: GetFiles = async ({
  folder
}: {
  next: string | null;
  maxResults: number;
  folder: string;
  userId: string;
}): Promise<StorageFilesList | null> => {
  const identity: Identity | undefined = getIdentity();

  const {bucket, actor}: {bucket: Principal; actor: StorageBucketActor} = await getStorageBucket({identity});

  const assets: AssetKey[] = await actor.list(toNullable<string>(folder));

  const host: string = `https://${bucket.toText()}.raw.ic0.app`;

  return {
    items: assets.map(({name, fullPath, token}: AssetKey) => ({
      downloadUrl: `${host}/${fullPath}?token=${token}`,
      fullPath,
      name
    })),
    nextPageToken: null
  };
};

export const deleteFile: DeleteFile = async ({downloadUrl, fullPath}: StorageFile): Promise<void> => {
  const identity: Identity | undefined = getIdentity();

  const {actor}: {bucket: Principal; actor: StorageBucketActor} = await getStorageBucket({identity});

  const {pathname}: URL = new URL(downloadUrl);
  const token: string = pathname.replace('?token=', '');

  return actor.del({fullPath, token});
};
