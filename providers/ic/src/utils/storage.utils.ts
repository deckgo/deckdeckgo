import {Identity} from '@dfinity/agent';
import {HeaderField, _SERVICE as StorageBucketActor} from '../canisters/storage/storage.did';
import {getIdentity} from '../providers/auth/auth.ic';
import {LogWindow} from '../types/sync.window';
import {toNullable} from './did.utils';
import {BucketActor, getStorageBucket} from './manager.utils';

export const upload = async ({
  data,
  filename,
  folder,
  storageActor,
  headers,
  token,
  fullPath: storagePath,
  log
}: {
  data: Blob;
  folder: string;
  filename: string;
  headers: HeaderField[];
  storageActor: StorageBucketActor;
  token?: string;
  fullPath?: string;
  log: LogWindow;
}): Promise<{fullPath: string; filename: string; token: string}> => {
  log({msg: `[upload] ${filename}: start`});
  const t0 = performance.now();

  const fullPath: string = storagePath || `/${folder}/${filename}`;

  const {batchId} = await storageActor.initUpload({
    name: filename,
    fullPath,
    token: toNullable<string>(token),
    folder
  });

  const t1 = performance.now();
  log({msg: `[upload] ${filename}: create batch`, duration: t1 - t0});

  const promises = [];

  const chunkSize = 700000;

  for (let start = 0; start < data.size; start += chunkSize) {
    const chunk: Blob = data.slice(start, start + chunkSize);

    promises.push(
      uploadChunk({
        batchId,
        chunk,
        storageActor
      })
    );
  }

  const chunkIds: {chunkId: bigint}[] = await Promise.all(promises);

  const t2 = performance.now();
  log({msg: `[upload] ${filename}: chunks`, duration: t2 - t1});

  await storageActor.commitUpload({
    batchId,
    chunkIds: chunkIds.map(({chunkId}: {chunkId: bigint}) => chunkId),
    headers: [['Content-Type', data.type], ['accept-ranges', 'bytes'], ...headers]
  });

  const t3 = performance.now();
  log({msg: `[upload] ${filename}: commit batch`, duration: t3 - t2});
  log({msg: `[upload] ${filename}: done`, duration: t3 - t0});

  return {
    fullPath,
    filename,
    token
  };
};

const uploadChunk = async ({
  batchId,
  chunk,
  storageActor
}: {
  batchId: bigint;
  chunk: Blob;
  storageActor: StorageBucketActor;
}): Promise<{chunkId: bigint}> =>
  storageActor.uploadChunk({
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
