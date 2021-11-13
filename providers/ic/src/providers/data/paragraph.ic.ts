import {Paragraph, ParagraphData} from '@deckdeckgo/editor';

import {getData} from '../../utils/data.utils';

export const getParagraph = (docId: string, paragraphId: string): Promise<Paragraph> =>
  getData<Paragraph, ParagraphData>({key: `/docs/${docId}/paragraphs/${paragraphId}`});
