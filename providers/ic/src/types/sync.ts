import {StorageFile} from '@deckdeckgo/editor';

export interface SyncICDeckBackground {
  imgSrc: string;
  deckId: string;
  storageFile: StorageFile;
}

export interface SyncICEvent {
  msg: 'deckdeckgo_sync_deck_background';
  data: SyncICDeckBackground;
}
