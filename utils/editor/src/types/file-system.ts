import {Deck} from '../models/data/deck';
import {Slide} from '../models/data/slide';

export interface FileImportData {
  id?: string;
  deck: Partial<Deck>;
  slides: Partial<Slide>[];
}

export interface UserAsset {
  key: string;
  blob: Blob;
  url?: string;
}
