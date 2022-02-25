import {Deck, Doc, Paragraph, Slide} from '@deckdeckgo/editor';

export interface ImportData {
  id: string;
  deck?: Deck;
  slides?: Slide[];
  doc?: Doc;
  paragraphs?: Paragraph[];
}

export interface ImportAsset {
  path: string;
  blob: Blob;
}
