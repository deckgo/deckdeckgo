import {StorageFilesList} from '../models/storage/storage';

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
