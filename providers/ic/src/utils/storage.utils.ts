import {Identity} from '@dfinity/agent';

import {_SERVICE as StorageBucketActor, HeaderField} from '../canisters/storage/storage.did';

import {toNullable} from './did.utils';

import {getIdentity} from '../providers/auth/auth.ic';

import {BucketActor, getStorageBucket} from './manager.utils';

export const upload = async ({
  data,
  filename,
  folder,
  storageBucket,
  headers,
  token,
  fullPath: storagePath
}: {
  data: Blob;
  folder: string;
  filename: string;
  headers: HeaderField[];
  storageBucket: StorageBucketActor;
  token?: string;
  fullPath?: string;
}): Promise<{fullPath: string; filename: string; token: string}> => {
  console.log('About to upload to the IC');
  const t0 = performance.now();

  const fullPath: string = storagePath || `/${folder}/${filename}`;

  const {batchId} = await storageBucket.initUpload({
    name: filename,
    fullPath,
    token: toNullable<string>(token),
    folder
  });

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

  await storageBucket.commitUpload({
    batchId,
    chunkIds: chunkIds.map(({chunkId}: {chunkId: bigint}) => chunkId),
    headers: [['Content-Type', data.type], ['accept-ranges', 'bytes'], ...headers]
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

const uploadChunk = async ({
  batchId,
  chunk,
  storageBucket
}: {
  batchId: bigint;
  chunk: Blob;
  storageBucket: StorageBucketActor;
}): Promise<{chunkId: bigint}> =>
  storageBucket.uploadChunk({
    batchId,
    content: [...new Uint8Array(await chunk.arrayBuffer())]
  });

export const encodeFilename = (filename: string): string => encodeURI(filename.toLowerCase().replace(/\s/g, '-'));

export const getStorageActor = async (): Promise<BucketActor<StorageBucketActor>> => {
  const identity: Identity | undefined = getIdentity();

  if (!identity) {
    throw new Error('No internet identity.');
  }

  const result: BucketActor<StorageBucketActor> = await getStorageBucket({identity});

  const {actor, bucketId} = result;

  if (!actor) {
    throw new Error('No actor initialized.');
  }

  // That would be strange
  if (!bucketId) {
    throw new Error('No bucket principal defined');
  }

  return result;
};
