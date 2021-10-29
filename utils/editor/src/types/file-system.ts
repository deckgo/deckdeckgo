import {Deck} from '../models/data/deck';
import {Slide} from '../models/data/slide';
import {Doc} from '../models/data/doc';
import {Paragraph} from '../models/data/paragraph';

export interface FileImportData {
  id?: string;
  deck?: Partial<Deck>;
  slides?: Partial<Slide>[];
  doc?: Partial<Doc>;
  paragraphs?: Partial<Paragraph>[];
}

export interface UserAsset {
  key: string;
  blob: Blob;
  url?: string;
}
