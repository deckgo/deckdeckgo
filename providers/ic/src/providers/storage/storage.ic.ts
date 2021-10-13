import {Identity} from '@dfinity/agent';
import {Principal} from '@dfinity/principal';

import {v4 as uuid} from 'uuid';

import {GetFiles, StorageFile, StorageFilesList} from '@deckdeckgo/editor';

import {getIdentity} from '../auth/auth.ic';

import {_SERVICE as ManagerActor} from '../../canisters/manager/manager.did';
import {_SERVICE as StorageBucketActor, AssetKey} from '../../canisters/storage/storage.did';

import {createManagerActor, createStorageBucketActor, initStorageBucket} from '../../utils/manager.utils';
import {toNullable} from '../../utils/did.utils';

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

  const {bucket, actor}: {bucket: Principal; actor: StorageBucketActor} = await getStorageBucket(host);

  const {fullPath, filename, token}: {fullPath: string; filename: string; token: string} = await upload({
    data,
    folder,
    storageBucket: actor
  });

  return {
    downloadUrl: `https://${bucket.toText()}.ic0.app/${fullPath}?token=${token}`,
    fullPath,
    name: filename
  };
};

const getStorageBucket = async (host?: string): Promise<{bucket: Principal; actor: StorageBucketActor}> => {
  const identity: Identity | undefined = getIdentity();

  if (!identity) {
    throw new Error('Invalid identity.');
  }

  const managerActor: ManagerActor = await createManagerActor({identity});

  const bucket: Principal = await initStorageBucket({managerActor});

  const actor: StorageBucketActor = await createStorageBucketActor({identity, bucket, host});

  return {
    bucket,
    actor
  };
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
}): Promise<{fullPath: string; filename: string; token: string}> => {
  const filename: string = encodeURI(data.name);
  const fullPath: string = `/${folder}/${filename}`;
  const token: string = uuid();

  const {batchId} = await storageBucket.create_batch({name: filename, fullPath, token, folder});

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
  const {bucket, actor}: {bucket: Principal; actor: StorageBucketActor} = await getStorageBucket();

  const assets: AssetKey[] = await actor.list(toNullable<string>(folder));

  const host: string = `https://${bucket.toText()}.ic0.app`;

  return {
    items: assets.map(({name, fullPath, token}: AssetKey) => ({
      downloadUrl: `${host}/${fullPath}?token=${token}`,
      fullPath,
      name
    })),
    nextPageToken: null
  };
};
