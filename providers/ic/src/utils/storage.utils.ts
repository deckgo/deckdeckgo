import {_SERVICE as StorageBucketActor} from '../canisters/storage/storage.did';

import {toNullable} from './did.utils';

export const upload = async ({
  data,
  filename,
  folder,
  storageBucket,
  token
}: {
  data: Blob;
  folder: string;
  filename: string;
  storageBucket: StorageBucketActor;
  token?: string;
}): Promise<{fullPath: string; filename: string; token: string}> => {
  const fullPath: string = `${folder}/${filename}`;

  console.log('About to upload to the IC');
  const t0 = performance.now();

  const {batchId} = await storageBucket.create_batch({name: filename, fullPath, token: toNullable<string>(token), folder});

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
