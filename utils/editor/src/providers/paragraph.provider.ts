import {Paragraph} from '../models/data/paragraph';

export interface GetParagraph {
  (docId: string, slideId: string): Promise<Paragraph | undefined>;
}

export interface UpdateParagraph {
  (docId: string, slide: Paragraph): Promise<Paragraph>;
}

export interface DeleteParagraph {
  (docId: string, slideId: string): Promise<void>;
}
