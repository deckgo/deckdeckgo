import {GetFiles, StorageFile, StorageFilesList, UploadFile} from '@deckdeckgo/editor';

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
