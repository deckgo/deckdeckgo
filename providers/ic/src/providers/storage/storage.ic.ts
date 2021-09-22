import {GetFiles, GetFolders, StorageFile, StorageFilesList, StorageFoldersList, UploadFile} from '@deckdeckgo/editor';

// TODO: implement storage for the internet computer

export const uploadFile: UploadFile = async (_param: {
  data: File;
  folder: string;
  maxSize: number;
  userId: string;
  downloadUrl?: boolean;
}): Promise<StorageFile | undefined> => {
  return undefined;
};

export const getFiles: GetFiles = async (_param: {
  next: string | null;
  maxResults: number;
  folder: string;
  userId: string;
}): Promise<StorageFilesList | null> => {
  return null;
};

export const getFolders: GetFolders = async (_param: {folder: string; userId: string}): Promise<StorageFoldersList | undefined> => {
  return undefined;
};
