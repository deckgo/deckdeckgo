import {Identity} from '@dfinity/agent';

import {v4 as uuid} from 'uuid';

import {GetFiles, StorageFile, StorageFilesList, UploadFile, DeleteFile} from '@deckdeckgo/editor';

import {getIdentity} from '../auth/auth.ic';

import {_SERVICE as StorageBucketActor, AssetKey} from '../../canisters/storage/storage.did';

import {toNullable} from '../../utils/did.utils';
import {BucketActor, getStorageBucket} from '../../utils/manager.utils';
import {encodeFilename, getStorageActor, upload} from '../../utils/storage.utils';

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
  identity,
  storageBucket
}: {
  data: File;
  folder: string;
  maxSize: number;
  host?: string;
  identity: Identity;
  storageBucket?: BucketActor<StorageBucketActor>;
}): Promise<StorageFile> => {
  if (!data || !data.name) {
    throw new Error('File not valid.');
  }

  if (data.size > maxSize) {
    throw new Error(`File is too big (max. ${maxSize / 1048576} Mb)`);
  }

  const {actor, bucketId}: BucketActor<StorageBucketActor> = storageBucket || (await getStorageBucket({host, identity}));

  if (!actor || !bucketId) {
    throw new Error('Storage bucket is not initialized.');
  }

  const {fullPath, filename, token}: {fullPath: string; filename: string; token: string} = await upload({
    data,
    filename: encodeFilename(data.name),
    folder,
    storageActor: actor,
    token: uuid(),
    headers: [['cache-control', 'private, max-age=0']]
  });

  return {
    downloadUrl: `https://${bucketId.toText()}.raw.ic0.app/${fullPath}?token=${token}`,
    fullPath,
    name: filename
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
  const {actor, bucketId}: BucketActor<StorageBucketActor> = await getStorageActor();

  const assets: AssetKey[] = await actor.list(toNullable<string>(folder));

  const host: string = `https://${bucketId.toText()}.raw.ic0.app`;

  return {
    items: assets.map(({name, fullPath, token}: AssetKey) => ({
      downloadUrl: `${host}${fullPath}?token=${token}`,
      fullPath,
      name
    })),
    nextPageToken: null
  };
};

export const deleteFile: DeleteFile = async ({downloadUrl, fullPath}: StorageFile): Promise<void> => {
  const {actor}: BucketActor<StorageBucketActor> = await getStorageActor();

  let token: string | null = null;

  if (downloadUrl) {
    const {searchParams}: URL = new URL(downloadUrl);
    token = searchParams.get('token');
  }

  return actor.del({fullPath, token: toNullable<string>(token ? token : undefined)});
};
