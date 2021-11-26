import {Identity} from '@dfinity/agent';
import {Principal} from '@dfinity/principal';

import {v4 as uuid} from 'uuid';

import {GetFiles, StorageFile, StorageFilesList, UploadFile, DeleteFile} from '@deckdeckgo/editor';

import {getIdentity} from '../auth/auth.ic';

import {_SERVICE as StorageBucketActor, AssetKey} from '../../canisters/storage/storage.did';

import {toNullable} from '../../utils/did.utils';
import {getStorageBucket} from '../../utils/manager.utils';
import {upload} from '../../utils/storage.utils';

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
}): Promise<StorageFile> => {
  if (!data || !data.name) {
    throw new Error('File not valid.');
  }

  if (data.size > maxSize) {
    throw new Error(`File is too big (max. ${maxSize / 1048576} Mb)`);
  }

  const {bucket, actor}: {bucket: Principal; actor: StorageBucketActor} = await getStorageBucket({host, identity});

  const {fullPath, filename, token}: {fullPath: string; filename: string; token: string} = await upload({
    data,
    filename: encodeURI(data.name),
    folder,
    storageBucket: actor,
    token: uuid()
  });

  return {
    downloadUrl: `https://${bucket.toText()}.raw.ic0.app/${fullPath}?token=${token}`,
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

  return actor.del({fullPath, token: toNullable<string>(token?.length > 0 ? token : undefined)});
};
