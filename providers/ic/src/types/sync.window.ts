import {StorageFile} from '@deckdeckgo/editor';
import {Log} from '@deckdeckgo/editor/lib';

export interface SyncWindowData {
  src: string;
  key: string;
  selector?: string;
  storageFile: StorageFile;
}

export type SyncWindowEventMsg =
  | 'deckdeckgo_sync_deck_background'
  | 'deckdeckgo_sync_slide_image'
  | 'deckdeckgo_sync_slide_chart'
  | 'deckdeckgo_sync_paragraph_image';

export interface SyncWindowEvent {
  msg: SyncWindowEventMsg;
  data: SyncWindowData;
}

export interface SyncWindow {
  ($event: SyncWindowEvent): Promise<void>;
}

export type LogWindow = (detail: Log) => void;

export type SignOutWindow = () => void;
