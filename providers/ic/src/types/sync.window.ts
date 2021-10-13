import {StorageFile} from '@deckdeckgo/editor';

export interface SyncWindowDeckBackground {
  imgSrc: string;
  deckId: string;
  storageFile: StorageFile;
}

export interface SyncWindow {
  syncDeckBackground: (data: SyncWindowDeckBackground) => Promise<void>;
}
