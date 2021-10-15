import {StorageFile} from '@deckdeckgo/editor';

export interface SyncStorage {
  src: string | undefined;
  storageFile: StorageFile | undefined;
}

export interface SyncStorageSlide {
  images: SyncStorage[] | undefined;
  chart: SyncStorage | undefined;
}
