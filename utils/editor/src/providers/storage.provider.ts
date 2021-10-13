import {StorageFile, StorageFilesList} from '../models/storage/storage';

export interface UploadFile {
  ({
    data,
    folder,
    maxSize,
    userId,
    downloadUrl
  }: {
    data: File;
    folder: string;
    maxSize: number;
    userId: string;
    downloadUrl?: boolean;
  }): Promise<StorageFile | undefined>;
}

export interface GetFiles {
  ({
    next,
    maxResults,
    folder,
    userId
  }: {
    next: string | null;
    maxResults: number;
    folder: string;
    userId: string;
  }): Promise<StorageFilesList | null>;
}
