import {Identity} from '@dfinity/agent';
import {Principal} from '@dfinity/principal';

import {GetFiles, StorageFile, StorageFilesList} from '@deckdeckgo/editor';

import {getIdentity} from '../auth/auth.ic';

import {_SERVICE as ManagerActor} from '../../canisters/manager/manager.did';
import {_SERVICE as StorageBucketActor} from '../../canisters/storage/storage.did';

import {createManagerActor, createStorageBucketActor, initStorageBucket} from '../../utils/manager.utils';

import {v4 as uuid} from 'uuid';

// TODO: studio should use offline upload and cron should interact with cloud and update DOM

export const uploadFile = async ({
  data,
  maxSize,
  host,
  folder
}: {
  data: File;
  folder: string;
  maxSize: number;
  host: string;
}): Promise<StorageFile | undefined> => {
  if (!data || !data.name) {
    throw new Error('File not valid.');
  }

  if (data.size > maxSize) {
    throw new Error(`File is too big (max. ${maxSize / 1048576} Mb)`);
  }

  const identity: Identity | undefined = getIdentity();

  if (!identity) {
    throw new Error('Invalid identity.');
  }

  const managerActor: ManagerActor = await createManagerActor({identity});

  const bucket: Principal = await initStorageBucket({managerActor});

  const storageBucket: StorageBucketActor = await createStorageBucketActor({identity, bucket, host});

  return upload({data, folder, storageBucket});
};

const uploadChunk = async ({batchId, chunk, storageBucket}: {batchId: bigint; chunk: Blob; storageBucket: StorageBucketActor}) =>
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
}): Promise<StorageFile> => {
  const filename: string = encodeURI(data.name);
  const path: string = `/${folder}/${filename}`;
  const token: string = uuid();

  const {batchId} = await storageBucket.create_batch({path, token});

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

  const chunkIds: bigint[] = await Promise.all(promises);

  await storageBucket.commit_batch({
    batchId,
    chunkIds,
    contentType: data.type
  });

  // TODO: return StorageFile

  return {
    downloadUrl: `https://..../${path}?token=${token}`,
    fullPath: `${path}?token=${token}`,
    name: filename
  };
};

export const getFiles: GetFiles = async (_param: {
  next: string | null;
  maxResults: number;
  folder: string;
  userId: string;
}): Promise<StorageFilesList | null> => {
  // TODO: list entries

  return null;
};
