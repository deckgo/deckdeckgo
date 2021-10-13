import {StorageFile} from '@deckdeckgo/editor';

export interface SyncWindowDeckBackground {
  imgSrc: string;
  deckId: string;
  storageFile: StorageFile;
}

export interface SyncWindowEvent {
  msg: 'deckdeckgo_sync_deck_background';
  data: SyncWindowDeckBackground;
}

export interface SyncWindow {
  ($event: SyncWindowEvent): Promise<void>;
}
